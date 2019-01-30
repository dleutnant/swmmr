#include <Rcpp.h>
using namespace Rcpp;

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
// [[Rcpp::export]]
List OpenSwmmOutFile(const char* outFile)
//-----------------------------------------------------------------------------
{

  int magic1, magic2, errCode, offset, offset0;
  int err;
  size_t size;
  
  // --- open the output file
  Fout = fopen(outFile, "rb");
  
  if (Fout == NULL) {

    printf("Could not open '%s' for binary reading.\n", outFile);
    
    return List::create(_["error"] = 2);
  }
  
  printf("File %s opened.\n", outFile);
  
  // --- check that file contains at least 14 records
  fseek(Fout, 0L, SEEK_END);
  
  // use ftello() instead of ftell():
  // https://stackoverflow.com/questions/16696297/ftell-at-a-position-past-2gb
  off_t ftell_result = ftello(Fout);
  
  if (ftell_result < 14 * RECORDSIZE) {
    
    fclose(Fout);
    
    err = 1;
    
    printf("File does not have at least 14 records.\n");
    printf("ftell returned: %jd\n", (intmax_t) ftell_result);
    printf("14 * RECORDSIZE is: %d\n", 14 * RECORDSIZE);
    
    return List::create(_["error"] = 3);
  }
  
  printf("File has at least 14 records.\n");

  // --- read parameters from end of file
  fseek(Fout, -5 * RECORDSIZE, SEEK_END);
  
  size = fread(&offset0, RECORDSIZE, 1, Fout);
  size = fread(&StartPos, RECORDSIZE, 1, Fout);
  size = fread(&SWMM_Nperiods, RECORDSIZE, 1, Fout);
  size = fread(&errCode, RECORDSIZE, 1, Fout);
  size = fread(&magic2, RECORDSIZE, 1, Fout);
  
  printf("offset0: %d\n", offset0);
  printf("StartPos: %d\n", StartPos);
  printf("SWMM_Nperiods: %d\n", SWMM_Nperiods);
  printf("errCode: %d\n", errCode);
  printf("magic2: %d\n", magic2);

  // --- read magic number from beginning of file
  fseek(Fout, 0L, SEEK_SET);
  
  size = fread(&magic1, RECORDSIZE, 1, Fout);

  printf("magic1: %d\n", magic1);
  
  // --- perform error checks
  err = (magic1 != magic2 || errCode != 0 || SWMM_Nperiods == 0)? 1:0;

  // --- quit if errors found
  if (err > 0 ) {
    
    fclose(Fout);
    Fout = NULL;
    
    return List::create(_["error"] = err);
  }
  else {
    
    printf("No error so far!\n");
  }
  
  // --- otherwise read additional parameters from start of file
  size = fread(&SWMM_version, RECORDSIZE, 1, Fout);
  printf("SWMM_version read: %d.\n", SWMM_version);
  
  size = fread(&SWMM_FlowUnits, RECORDSIZE, 1, Fout);
  printf("SWMM_FlowUnits read: %d\n", SWMM_FlowUnits);
  
  size = fread(&SWMM_Nsubcatch, RECORDSIZE, 1, Fout);  
  printf("SWMM_Nsubcatch read: %d\n", SWMM_Nsubcatch);

  size = fread(&SWMM_Nnodes, RECORDSIZE, 1, Fout);
  printf("SWMM_Nnodes read: %d\n", SWMM_Nnodes);
  
  size = fread(&SWMM_Nlinks, RECORDSIZE, 1, Fout);
  printf("SWMM_Nlinks read: %d\n", SWMM_Nlinks);
  
  size = fread(&SWMM_Npolluts, RECORDSIZE, 1, Fout);
  printf("SWMM_Npolluts read: %d\n", SWMM_Npolluts);

  // dummy variable to store arrays of chars
  char buffer[80]; 

  // --- extract subcatchment names
  std::vector<int> IDsubcatch(SWMM_Nsubcatch);
  std::vector<std::string> Namesubcatch(SWMM_Nsubcatch);

  for (int i = 1; i <= SWMM_Nsubcatch; ++i) {
    
    size = fread(&IDsubcatch[i - 1], RECORDSIZE, 1, Fout);
    Namesubcatch[i-1] = fgets(buffer, IDsubcatch[i-1]+1, Fout);
  }
  
  printf("Subcatchments read.\n");

  //return List::create(_["ok"] = 1);

  // --- extract node names
  std::vector<int>  IDnodes(SWMM_Nnodes);
  std::vector<std::string> Namenodes(SWMM_Nnodes);
  for ( int i=1; i<=SWMM_Nnodes; ++i) 
  {
    size = fread(&IDnodes[i-1], RECORDSIZE, 1, Fout);
    Namenodes[i-1] = fgets(buffer, IDnodes[i-1]+1, Fout); 
  }

  printf("Nodes read.\n");
  
  // --- extract link names
  std::vector<int> IDlinks(SWMM_Nlinks);
  std::vector<std::string> Namelinks(SWMM_Nlinks);
  for ( int i=1; i<=SWMM_Nlinks; ++i) 
  {
    size = fread(&IDlinks[i-1], RECORDSIZE, 1, Fout);
    Namelinks[i-1] = fgets(buffer, IDlinks[i-1]+1, Fout); 
  }

  printf("Links read.\n");
  
  // --- extract pollutant names
  std::vector<int> IDpolls(SWMM_Npolluts);
  std::vector<std::string> Namepolls(SWMM_Npolluts);
  
  for ( int i=1; i<=SWMM_Npolluts; ++i) 
  {
    size = fread(&IDpolls[i-1], RECORDSIZE, 1, Fout);
    Namepolls[i-1] = fgets(buffer, IDpolls[i-1]+1, Fout); 
  }

  printf("Pollutants read.\n");

  // Skip over saved subcatch/node/link input values
  offset = (SWMM_Nsubcatch+2) * RECORDSIZE  // Subcatchment area
    + (3*SWMM_Nnodes+4) * RECORDSIZE  // Node type, invert & max depth
    + (5*SWMM_Nlinks+6) * RECORDSIZE; // Link type, z1, z2, max depth & length
    offset = offset0 + offset;
    fseek(Fout, offset, SEEK_SET);
    
    // Read number & codes of computed variables
    size = fread(&SubcatchVars, RECORDSIZE, 1, Fout); // # Subcatch variables
    fseek(Fout, SubcatchVars*RECORDSIZE, SEEK_CUR);
    size = fread(&NodeVars, RECORDSIZE, 1, Fout);     // # Node variables
    fseek(Fout, NodeVars*RECORDSIZE, SEEK_CUR);
    size = fread(&LinkVars, RECORDSIZE, 1, Fout);     // # Link variables
    fseek(Fout, LinkVars*RECORDSIZE, SEEK_CUR);
    size = fread(&SysVars, RECORDSIZE, 1, Fout);     // # System variables
    
    // --- read data just before start of output results
    offset = StartPos - 3*RECORDSIZE;
    fseek(Fout, offset, SEEK_SET);
    size = fread(&SWMM_StartDate, sizeof(double), 1, Fout);
    size = fread(&SWMM_ReportStep, RECORDSIZE, 1, Fout);
    
    // --- compute number of bytes of results values used per time period
    BytesPerPeriod = 2*RECORDSIZE +      // date value (a double)
      (SWMM_Nsubcatch*SubcatchVars +
      SWMM_Nnodes*NodeVars+
      SWMM_Nlinks*LinkVars +
      SysVars)*RECORDSIZE;
    
    // --- return with file left open
    return List::create(_["meta"] =  List::create(_["version"] = SWMM_version),
                        _["subcatchments"] = List::create(_["names"] = Namesubcatch),
                        _["nodes"] = List::create(_["names"] = Namenodes),
                        _["links"] = List::create(_["names"] = Namelinks),
                        _["pollutants"] = List::create(_["names"] = Namepolls));

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
  int offset;
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
    
    offset = StartPos + (i - 1) * BytesPerPeriod + 2 * RECORDSIZE;

    skip = 0 +
      ((iType > SUBCATCH) ? SWMM_Nsubcatch * SubcatchVars : 0) +
      ((iType > NODE)     ? SWMM_Nnodes    * NodeVars     : 0) +
      ((iType > LINK)     ? SWMM_Nlinks    * LinkVars     : 0);

    vars = (iType == SUBCATCH)? SubcatchVars : 
      (iType == NODE) ? NodeVars :
      (iType == LINK) ? LinkVars :
      (iType == SYS) ? SysVars : -1;

    offset += RECORDSIZE * (skip + iIndex * vars + vIndex);

    // --- re-position the file and read the result
    fseek(Fout, offset, SEEK_SET);
    
    size = fread(&resultvec[i - firstPeriod], RECORDSIZE, 1, Fout);
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
  Rcpp::NumericVector timesvec(SWMM_Nperiods);
  
  for ( int i=1; i<=SWMM_Nperiods; ++i)
  {
    timesvec[i-1] = SWMM_StartDate*86400 + SWMM_ReportStep*i ;
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
