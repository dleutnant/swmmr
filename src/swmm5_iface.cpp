#include <Rcpp.h>
using namespace Rcpp;

#define ERROR_FILE_OPEN 1
#define ERROR_FILE_SEEK 2
#define ERROR_FILE_TELL 3
#define MAX_VAR_VALUES 15
#define N_INPUT_RECORDS(x, n) (1 + n * (x + 1))

// dummy variable to store array of chars
char buffer[80]; 

int    SWMM_version;                   // SWMM version
int    SWMM_Nperiods;                  // number of reporting periods
int    SWMM_FlowUnits;                 // flow units code
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

static int n_objects[4]; // SUBCATCH, NODE, LINK, SYS
static int n_records[4]; // SUBCATCH, NODE, LINK, SYS

static int n_records_skip[4]; // SUBCATCH, NODE, LINK, SYS

// number of reporting variables
static int n_variables[4]; // SUBCATCH, NODE, LINK, SYS

static FILE*  Fout;                    // file handle
static int    OutputStartPos;          // file position where results start
static double BytesPerPeriod;          // bytes used for results in each period

// Variable values of one object (SUBCATCH, NODE, LINK, SYS) 
// at one point in time
static float var_values[MAX_VAR_VALUES];

//-----------------------------------------------------------------------------
int open_output_file(const char* outFile)
{
  if ((Fout = fopen(outFile, "rb")) == NULL) {
    
    Rprintf("Could not open '%s' for binary reading.\n", outFile);
    
  } else {
    
    Rprintf("File %s opened.\n", outFile);
  }

  return (Fout != NULL);
}

//-----------------------------------------------------------------------------
int file_seek(off_t offset, int whence)
{
  /*if (debug) Rprintf(
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
  //Rprintf("Reading %s ... ", name);
  
  size_t size = fread(pointer, n_bytes, 1, Fout);

  if (size != 1) {
    
    Rprintf("Reading %s failed.\n", name);
    
    return 0;
  }
  
  //Rprintf("ok.\n");

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
    
    Rprintf("File is not as big as expected.\n");
    Rprintf("- ftello returned: %jd\n", (intmax_t) result);
    Rprintf("- least_expected: %jd\n", (intmax_t) least_expected);
    
    return 0;
  }
  
  return 1;
}

//-----------------------------------------------------------------------------
int read_names(std::vector<std::string> &names, const char* type_name)
{
  int n_chars;
  
  // found here: https://stackoverflow.com/questions/31196443/
  //   how-can-i-get-the-size-of-an-stdvector-as-an-int
  int size = static_cast<int>(names.size());
  
  for (int i = 0; i < size; i++) {

    if (! read_record(&n_chars, type_name)) {
      return 0;
    }

    names[i] = fgets(buffer, n_chars + 1, Fout);
  }
  
  Rprintf("%d %ss read.\n", size, type_name);
  
  return 1;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
List OpenSwmmOutFile(const char* outFile)
{
  int magic1, magic2, errCode, InputStartPos;
  off_t offset;

  // --- open the output file
  if (! open_output_file(outFile)) {
    return List::create(_["error"] = ERROR_FILE_OPEN);
  }
  
  // --- check that file contains at least 14 records
  if (! file_seek(0, SEEK_END)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  if (! file_tell(14 * RECORDSIZE)) {
    return List::create(_["error"] = ERROR_FILE_TELL);
  }

  // --- read parameters from end of file
  if (! file_seek(-5 * RECORDSIZE, SEEK_END)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }

  read_record(&InputStartPos, "InputStartPos");
  read_record(&OutputStartPos, "OutputStartPos");
  read_record(&SWMM_Nperiods, "SWMM_Nperiods");
  read_record(&errCode, "errCode");
  read_record(&magic2, "magic2");

  Rprintf("InputStartPos: %d\n", InputStartPos);   // in SWMM: InputStartPos
  Rprintf("OutputStartPos: %d\n", OutputStartPos); // in SWMM: OutputStartPos
  Rprintf("SWMM_Nperiods: %d\n", SWMM_Nperiods);
  Rprintf("errCode: %d\n", errCode);
  Rprintf("magic2: %d\n", magic2);
  
  // --- read magic number from beginning of file
  if (! file_seek(0, SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }

  read_record(&magic1, "magic1");

  // --- quit if errors found
  if (int error = (magic1 != magic2 || errCode != 0 || SWMM_Nperiods == 0)) {
    return List::create(_["error"] = error);
  }

  // --- otherwise read additional parameters from start of file
  read_record(&SWMM_version, "SWMM_version");
  read_record(&SWMM_FlowUnits, "SWMM_FlowUnits");
  
  read_record(&(n_objects[SUBCATCH]), "n_objects[SUBCATCH]");
  read_record(&(n_objects[NODE]), "n_objects[NODE]");
  read_record(&(n_objects[LINK]), "n_objects[LINK]");
  
  read_record(&SWMM_Npolluts, "SWMM_Npolluts");

  // --- define name vectors
  std::vector<std::string> Namesubcatch(n_objects[SUBCATCH]);
  std::vector<std::string> Namenodes(n_objects[NODE]);
  std::vector<std::string> Namelinks(n_objects[LINK]);
  std::vector<std::string> Namepolls(SWMM_Npolluts);
  
  // --- read object names from file
  read_names(Namesubcatch, "subcatchment");
  read_names(Namenodes, "node");
  read_names(Namelinks, "link");
  read_names(Namepolls, "pollutant");
  
  // Skip over saved subcatch/node/link input values
  offset = (off_t) InputStartPos + (off_t) RECORDSIZE * (
    N_INPUT_RECORDS(n_objects[SUBCATCH], 1) + // Subcatchment area
    N_INPUT_RECORDS(n_objects[NODE], 3) + // Node type, invert & max depth
    N_INPUT_RECORDS(n_objects[LINK], 5) // Link type, z1, z2, max depth & length
  );
  
  if (! file_seek(offset, SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  // Read number & codes of computed variables
  //read_record(&SubcatchVars, "SubcatchVars"); // # Subcatch variables
  read_record(&(n_variables[SUBCATCH]), "n_variables[SUBCATCH]"); // # Subcatch variables

  if (! file_seek(n_variables[SUBCATCH] * RECORDSIZE, SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&(n_variables[NODE]), "n_variables[NODE]"); // # Node variables
  
  if (! file_seek(n_variables[NODE] * RECORDSIZE, SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&(n_variables[LINK]), "n_variables[LINK]"); // # Link variables
  
  if (! file_seek(n_variables[LINK] * RECORDSIZE, SEEK_CUR)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }
  
  read_record(&(n_variables[SYS]), "n_variables[SYS]"); // # System variables
  
  // --- read data just before start of output results
  if (! file_seek(OutputStartPos - 3 * RECORDSIZE, SEEK_SET)) {
    return List::create(_["error"] = ERROR_FILE_SEEK);
  }

  read_record_double(&SWMM_StartDate, "SWMM_StartDate");
  read_record(&SWMM_ReportStep, "SWMM_ReportStep");

  Rprintf("n_variables[SUBCATCH]: %d\n", n_variables[SUBCATCH]);
  Rprintf("n_variables[NODE]: %d\n", n_variables[NODE]);
  Rprintf("n_variables[LINK]: %d\n", n_variables[LINK]);
  Rprintf("n_variables[SYS]: %d\n", n_variables[SYS]);
  
  Rprintf("SWMM_StartDate: %f\n", SWMM_StartDate);
  Rprintf("SWMM_ReportStep: %d\n", SWMM_ReportStep);

  // calculate helper variables that will be used to determine the position
  // of result data in the output file 
  int n_record_sum = 0;

  // Set the number of system "objects" to 1, just to be consistent and to be 
  // able to let the following loop go from 0 (SUBCATCH) to 3 (SYS)
  n_objects[SYS] = 1;
  
  for (int i = SUBCATCH; i <= SYS; i++) {

    n_records[i] = n_objects[i] * n_variables[i];
    n_record_sum += n_records[i];
    
    if (i > SUBCATCH) {
      n_records_skip[i] = n_records_skip[i - 1] + n_records[i - 1];      
    } 
    else {
      n_records_skip[i] = 0;
    }
  }
  
  // --- compute number of bytes of results values used per time period
  // date value (a double)
  BytesPerPeriod = RECORDSIZE * (2 + n_record_sum);
 
  // --- return with file left open
  return List::create(
    _["meta"] = List::create(_["version"] = SWMM_version),
    _["subcatchments"] = List::create(_["names"] = Namesubcatch),
    _["nodes"] = List::create(_["names"] = Namenodes),
    _["links"] = List::create(_["names"] = Namelinks),
    _["pollutants"] = List::create(_["names"] = Namepolls)
  );
}

//-----------------------------------------------------------------------------
int restrict_to_range(int i, int from, int to, const char* name) {
  
  if (i < from) {
    Rprintf("Setting %s (%d) to min allowed value: %d\n", name, i, from);
    i = from;
  } 
  else if (i > to) {
    Rprintf("Setting %s (%d) to max allowed value: %d\n", name, i, to);
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
  int n = SWMM_Nperiods;

  firstPeriod = restrict_to_range(firstPeriod, 1, n, "firstPeriod");
  lastPeriod = restrict_to_range(lastPeriod, firstPeriod, n, "lastPeriod");
  
  std::vector<float> resultvec(lastPeriod - firstPeriod + 1);

  if (iType != SUBCATCH && iType != NODE && iType != LINK && iType != SYS) {
    Rprintf("Unknown iType: %d\n", iType);
    return wrap(resultvec);
  }
  
  if (iType == SYS) {
    if (iIndex != 777) {
      Rprintf(
        "iIndex is not 777 as expected but: %d. Anyway using iIndex = 0.",
        iIndex
      );
    }
    iIndex = 0;
  }
  
  // --- compute offset into output file
  offset = (off_t) OutputStartPos + RECORDSIZE * (
    2 + n_records_skip[iType] + iIndex * n_variables[iType] + vIndex
  );
  
  if (! file_seek(offset, SEEK_SET)) {
    return wrap(resultvec);
  }
  
  for (int i = firstPeriod; i <= lastPeriod; ++i) {

    // --- re-position the file and read the result
    if (! file_seek(offset + (off_t) (i - 1) * BytesPerPeriod, SEEK_SET)) {
      return wrap(resultvec);
    }
    
    read_record(&resultvec[i - firstPeriod], "resultvec");
  }
  
  return wrap(resultvec);
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector GetSwmmResultPart2(
    int iType, int iIndex, 
    Rcpp::IntegerVector varIndices,
    int firstPeriod, int lastPeriod
)
{
  off_t offset;
  int n = SWMM_Nperiods;

  firstPeriod = restrict_to_range(firstPeriod, 1, n, "firstPeriod");
  lastPeriod = restrict_to_range(lastPeriod, firstPeriod, n, "lastPeriod");

  // Count number of variables
  int nVars = varIndices.size();

  Rprintf("Number of variables to read: %d\n", nVars);
  
  std::vector<float> resultvec(nVars * (lastPeriod - firstPeriod + 1));

  if (iType != SUBCATCH && iType != NODE && iType != LINK && iType != SYS) {
    Rprintf("Unknown iType: %d\n", iType);
    return wrap(resultvec);
  }
  
  if (iType == SYS) {
    if (iIndex != 777) {
      Rprintf(
        "iIndex is not 777 as expected but: %d. Anyway using iIndex = 0.",
        iIndex
      );
    }
    iIndex = 0;
  }
  
  // --- compute offset into output file
  offset = (off_t) OutputStartPos + RECORDSIZE * (
    2 + n_records_skip[iType] + iIndex * n_variables[iType]
  );
  
  if (! file_seek(offset, SEEK_SET)) {
    return wrap(resultvec);
  }

  for (int i = firstPeriod; i <= lastPeriod; ++i) {
    
    // --- re-position the file and read the result
    if (! file_seek(offset + (off_t) (i - 1) * BytesPerPeriod, SEEK_SET)) {
      return wrap(resultvec);
    }

    size_t size = fread(var_values, RECORDSIZE, MAX_VAR_VALUES, Fout);
    
    if (size != MAX_VAR_VALUES) {
      
      Rprintf(
        "Could not read %d values from the output file.\n", MAX_VAR_VALUES
      );
      
      return wrap(resultvec);
    }

    // Copy values into result vector
    for (int j = 0; j < nVars; j++) {
      resultvec[nVars * (i - firstPeriod) + j] = var_values[varIndices[j]];
    }
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
{
  // Initialise vector with all elements equal to start date in seconds 
  Rcpp::NumericVector timesvec(SWMM_Nperiods, SWMM_StartDate * 86400);

  // Add a multiple of the report time step to each element
  for (int i = 0; i < SWMM_Nperiods; i++) {
    
    timesvec[i] += (double) SWMM_ReportStep * (i + 1);
  }
  
  return timesvec;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
int CloseSwmmOutFile()
{
  if (Fout != NULL) {
    fclose(Fout);
    Fout = NULL;
    return 1;
  }
  
  return 0;
}
