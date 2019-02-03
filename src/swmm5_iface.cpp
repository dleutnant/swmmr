#include <Rcpp.h>
using namespace Rcpp;

#define ERROR_FILE_OPEN 1
#define ERROR_FILE_SEEK 2
#define ERROR_FILE_TELL 3

int    SWMM_version;                   // SWMM version
int    SWMM_Nperiods;                  // number of reporting periods
int    SWMM_FlowUnits;                 // flow units code
int    SWMM_Nsubcatch;                 // number of subcatchments
int    SWMM_Nnodes;                    // number of drainage system nodes
int    SWMM_Nlinks;                    // number of drainage system links
int    SWMM_Npolluts;                  // number of pollutants tracked
double SWMM_StartDate;                 // start date of simulation
int    SWMM_ReportStep;                // reporting time step (seconds)

//int    RunSwmmExe(char* cmdLine);
//int    RunSwmmDll(char* inpFile, char* rptFile, char* outFile);
//int    OpenSwmmOutFile(char* outFile);
//int    GetSwmmResult(int iType, int iIndex, int vIndex, int period);
//int    CloseSwmmOutFile();

static const int SUBCATCH = 0;
static const int NODE     = 1;
static const int LINK     = 2;
static const int SYS      = 3;
static const int RECORDSIZE = 4;       // number of bytes per file record

static int SubcatchVars;               // number of subcatch reporting variables
static int NodeVars;                   // number of node reporting variables
static int LinkVars;                   // number of link reporting variables
static int SysVars;                    // number of system reporting variables

static FILE*  Fout;                    // file handle
static int    StartPos;                // file position where results start
static double BytesPerPeriod;          // bytes used for results in each period

//-----------------------------------------------------------------------------
int open_output_file(const char* outFile)
{
  if ((Fout = fopen(outFile, "rb")) == NULL) {
    
    printf("Could not open '%s' for binary reading.\n", outFile);
    
  } else {
    
    printf("File %s opened.\n", outFile);
  }

  return (Fout != NULL);
}

//-----------------------------------------------------------------------------
int file_seek(off_t offset, int whence)
{
  /*if (debug) printf(
    "fseeko(Fout, offset = %jd, whence = %d) ... \n",
    (intmax_t) offset, whence
  );*/
  
  if (fseeko(Fout, offset, whence) != 0) {
    
    return 0;
  }
  
  return 1;
}

//-----------------------------------------------------------------------------
int read_bytes(void* pointer, const char* name, int n_bytes)
{
  //printf("Reading %s ... ", name);
  
  size_t size = fread(pointer, n_bytes, 1, Fout);

  if (size != 1) {
    
    printf("Reading %s failed.\n", name);
    
    return 0;
  }
  
  //printf("ok.\n");

  return 1;
}

//-----------------------------------------------------------------------------
int read_record(void* pointer, const char* name)
{
  return read_bytes(pointer, name, RECORDSIZE);
}

//-----------------------------------------------------------------------------
int read_record_double(void* pointer, const char* name)
{
  return read_bytes(pointer, name, sizeof(double));
}  

//-----------------------------------------------------------------------------
int file_tell(off_t least_expected)
{
  // use ftello() instead of ftell():
  // https://stackoverflow.com/questions/16696297/ftell-at-a-position-past-2gb
  off_t result = ftello(Fout);
  
  if (result < least_expected) {
    
    printf("File is not as big as expected.\n");
    printf("- ftello returned: %jd\n", (intmax_t) result);
    printf("- least_expected: %jd\n", (intmax_t) least_expected);
    
    return 0;
  }
  
  return 1;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
List OpenSwmmOutFile(const char* outFile)
  //-----------------------------------------------------------------------------
{
  int magic1, magic2, errCode, offset0, err;
  off_t return_value, offset;
  size_t size;

  // --- open the output file
  if (! open_output_file(outFile)) {
    return List::create(_["error"] = ERROR_FILE_OPEN);
  }
  
  // --- check that file contains at least 14 records
  if (! file_seek((off_t) 0, SEEK_END)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  if (! file_tell((off_t) 14 * RECORDSIZE)) {
    return List::create(_["error"] = ERROR_FILE_TELL);
  }

  printf("File has at least 14 records.\n");
  
  // --- read parameters from end of file
  if (! file_seek((off_t) -5 * RECORDSIZE, SEEK_END)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }

  read_record(&offset0, "offset0");
  read_record(&StartPos, "StartPos");
  read_record(&SWMM_Nperiods, "SWMM_Nperiods");
  read_record(&errCode, "errCode");
  read_record(&magic2, "magic2");

  printf("offset0: %d\n", offset0);
  printf("StartPos: %d\n", StartPos);
  printf("SWMM_Nperiods: %d\n", SWMM_Nperiods);
  printf("errCode: %d\n", errCode);
  printf("magic2: %d\n", magic2);
  
  // --- read magic number from beginning of file
  if (! file_seek((off_t) 0, SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&magic1, "magic1");

  // --- perform error checks
  err = (magic1 != magic2 || errCode != 0 || SWMM_Nperiods == 0)? 1:0;
  
  // --- quit if errors found
  if (err > 0 ) {
    
    fclose(Fout);
    Fout = NULL;
    
    return List::create(_["error"] = err);
  }

  // --- otherwise read additional parameters from start of file
  read_record(&SWMM_version, "SWMM_version");
  read_record(&SWMM_FlowUnits, "SWMM_FlowUnits");
  read_record(&SWMM_Nsubcatch, "SWMM_Nsubcatch");
  read_record(&SWMM_Nnodes, "SWMM_Nnodes");
  read_record(&SWMM_Nlinks, "SWMM_Nlinks");
  read_record(&SWMM_Npolluts, "SWMM_Npolluts");

  // dummy variable to store arrays of chars
  char buffer[80]; 
  
  // --- extract subcatchment names
  std::vector<int> IDsubcatch(SWMM_Nsubcatch);
  std::vector<std::string> Namesubcatch(SWMM_Nsubcatch);
  
  for (int i = 1; i <= SWMM_Nsubcatch; ++i) {
    
    read_record(&IDsubcatch[i - 1], "subcatchment");
    Namesubcatch[i - 1] = fgets(buffer, IDsubcatch[i - 1] + 1, Fout);
  }
  
  printf("%d subcatchments read.\n", SWMM_Nsubcatch);
  
  //return List::create(_["ok"] = 1);
  
  // --- extract node names
  std::vector<int>  IDnodes(SWMM_Nnodes);
  std::vector<std::string> Namenodes(SWMM_Nnodes);
  for (int i = 1; i <= SWMM_Nnodes; ++i) 
  {
    read_record(&IDnodes[i - 1], "node");
    Namenodes[i - 1] = fgets(buffer, IDnodes[i - 1] + 1, Fout); 
  }
  
  printf("%d nodes read.\n", SWMM_Nnodes);
  
  // --- extract link names
  std::vector<int> IDlinks(SWMM_Nlinks);
  std::vector<std::string> Namelinks(SWMM_Nlinks);
  for (int i = 1; i <= SWMM_Nlinks; ++i) 
  {
    read_record(&IDlinks[i - 1], "link");
    Namelinks[i - 1] = fgets(buffer, IDlinks[i - 1] + 1, Fout); 
  }
  
  printf("%d links read.\n", SWMM_Nlinks);
  
  // --- extract pollutant names
  std::vector<int> IDpolls(SWMM_Npolluts);
  std::vector<std::string> Namepolls(SWMM_Npolluts);
  
  for (int i = 1; i <= SWMM_Npolluts; ++i) 
  {
    read_record(&IDpolls[i - 1], "poll");
    Namepolls[i - 1] = fgets(buffer, IDpolls[i - 1] + 1, Fout); 
  }
  
  printf("%d pollutants read.\n", SWMM_Npolluts);
  
  // Skip over saved subcatch/node/link input values
  offset = (off_t) offset0 + (off_t) RECORDSIZE * (
    (2 + 1 * SWMM_Nsubcatch) +  // Subcatchment area
    (4 + 3 * SWMM_Nnodes) + // Node type, invert & max depth
    (6 + 5 * SWMM_Nlinks) // Link type, z1, z2, max depth & length
  );
  
  if (! file_seek(offset, SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  // Read number & codes of computed variables
  read_record(&SubcatchVars, "SubcatchVars"); // # Subcatch variables
  
  if (! file_seek((off_t) (SubcatchVars * RECORDSIZE), SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&NodeVars, "NodeVars"); // # Node variables
  
  if (! file_seek((off_t) (NodeVars) * RECORDSIZE, SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&LinkVars, "LinkVars"); // # Link variables
  
  if (! file_seek((off_t) (LinkVars * RECORDSIZE), SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&SysVars, "SysVars"); // # System variables
  
  // --- read data just before start of output results
  if (! file_seek((off_t) (StartPos - 3 * RECORDSIZE), SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }

  read_record_double(&SWMM_StartDate, "SWMM_StartDate");
  read_record(&SWMM_ReportStep, "SWMM_ReportStep");

  printf("SubcatchVars: %d\n", SubcatchVars);
  printf("NodeVars: %d\n", NodeVars);
  printf("LinkVars: %d\n", LinkVars);
  printf("SysVars: %d\n", SysVars);
  printf("SWMM_StartDate: %f\n", SWMM_StartDate);
  printf("SWMM_ReportStep: %d\n", SWMM_ReportStep);
  
  // --- compute number of bytes of results values used per time period
  // date value (a double)
  BytesPerPeriod = RECORDSIZE * (
    SWMM_Nsubcatch * SubcatchVars +
      SWMM_Nnodes * NodeVars +
      SWMM_Nlinks * LinkVars +
      SysVars + 2
  );
  
  // --- return with file left open
  return List::create(
    _["meta"] = List::create(_["version"] = SWMM_version),
    _["subcatchments"] = List::create(_["names"] = Namesubcatch),
    _["nodes"] = List::create(_["names"] = Namenodes),
    _["links"] = List::create(_["names"] = Namelinks),
    _["pollutants"] = List::create(_["names"] = Namepolls)
  );
}

int restrict_to_range(int i, int from, int to, const char* name) {
  
  if (i < from) {
    printf("Setting %s (%d) to min allowed value: %d\n", name, i, from);
    i = from;
  } 
  else if (i > to) {
    printf("Setting %s (%d) to max allowed value: %d\n", name, i, to);
    i = to;
  }
  
  return i;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector GetSwmmResultPart(
    int iType, int iIndex, int vIndex, int firstPeriod, int lastPeriod
)
{
  off_t offset;
  int skip;
  int vars;

  firstPeriod = restrict_to_range(firstPeriod, 1, SWMM_Nperiods, "firstPeriod");
  lastPeriod = restrict_to_range(lastPeriod, firstPeriod, SWMM_Nperiods, "lastPeriod");
  
  std::vector<float> resultvec(lastPeriod - firstPeriod + 1);
  size_t size;
  
  if (iType != SUBCATCH && iType != NODE && iType != LINK && iType != SYS) {
    return wrap(resultvec);
  }
  
  // --- compute offset into output file
  
  for (int i = firstPeriod; i <= lastPeriod; ++i) {
    
    skip = 0 +
      ((iType > SUBCATCH) ? SWMM_Nsubcatch * SubcatchVars : 0) +
      ((iType > NODE)     ? SWMM_Nnodes    * NodeVars     : 0) +
      ((iType > LINK)     ? SWMM_Nlinks    * LinkVars     : 0);
    
    vars = (iType == SUBCATCH)? SubcatchVars : 
      (iType == NODE) ? NodeVars :
      (iType == LINK) ? LinkVars :
      (iType == SYS) ? SysVars : -1;

    offset = (off_t) StartPos;
    offset += (off_t) (i - 1) * (off_t) BytesPerPeriod + 2 * RECORDSIZE;
    offset += (off_t) (RECORDSIZE * (skip + iIndex * vars + vIndex));
    
    // --- re-position the file and read the result
    if (! file_seek(offset, SEEK_SET)) {
      return wrap(resultvec);
    }

    read_record(&resultvec[i - firstPeriod], "resultvec");
  }
  
  return wrap(resultvec);
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector GetSwmmResult(int iType, int iIndex, int vIndex)
{
  return GetSwmmResultPart(iType, iIndex, vIndex, 1, SWMM_Nperiods);
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector GetSwmmTimes()
//-----------------------------------------------------------------------------
{
  Rcpp::NumericVector timesvec(SWMM_Nperiods, SWMM_StartDate * 86400);

  for (int i = 0; i < SWMM_Nperiods; i++) {
    
    timesvec[i] += (double) SWMM_ReportStep * (i + 1);
  }
  
  return timesvec;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
int CloseSwmmOutFile()
  //-----------------------------------------------------------------------------
{
  if (Fout != NULL) {
    fclose(Fout);
    Fout = NULL;
    return 1;
  }
  
  return 0;
}
