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
  
  // --- open the output file
  Fout = fopen(outFile, "rb");
  if (Fout == NULL) return List::create(_["error"] = 2);
  
  // --- check that file contains at least 14 records
  fseek(Fout, 0L, SEEK_END);
  if (ftell(Fout) < 14*RECORDSIZE)
  {
    fclose(Fout);
    err = 1;
    return List::create(_["error"] = 3);
  }
  
  // --- read parameters from end of file
  fseek(Fout, -5*RECORDSIZE, SEEK_END);
  fread(&offset0, RECORDSIZE, 1, Fout);
  fread(&StartPos, RECORDSIZE, 1, Fout);
  fread(&SWMM_Nperiods, RECORDSIZE, 1, Fout);
  fread(&errCode, RECORDSIZE, 1, Fout);
  fread(&magic2, RECORDSIZE, 1, Fout);
  
  // --- read magic number from beginning of file
  fseek(Fout, 0L, SEEK_SET);
  fread(&magic1, RECORDSIZE, 1, Fout);
  
  // --- perform error checks
  if (magic1 != magic2) err = 1;
  else if (errCode != 0) err = 1;
  else if (SWMM_Nperiods == 0) err = 1;
  else err = 0;
  
  // --- quit if errors found
  if (err > 0 )
  {
    fclose(Fout);
    Fout = NULL;
    return List::create(_["error"] = err);
  }
  
  // --- otherwise read additional parameters from start of file
  fread(&SWMM_version, RECORDSIZE, 1, Fout);
  fread(&SWMM_FlowUnits, RECORDSIZE, 1, Fout);
  fread(&SWMM_Nsubcatch, RECORDSIZE, 1, Fout);
  fread(&SWMM_Nnodes, RECORDSIZE, 1, Fout);
  fread(&SWMM_Nlinks, RECORDSIZE, 1, Fout);
  fread(&SWMM_Npolluts, RECORDSIZE, 1, Fout);
  
  // dummy variable to store arrays of chars
  char tmp; 
 
  // --- extract subcatchment names
  std::vector<int> IDsubcatch(SWMM_Nsubcatch);
  std::vector<std::string> Namesubcatch(SWMM_Nsubcatch);
  for ( int i=1; i<=SWMM_Nsubcatch; ++i) 
    {
    fread(&IDsubcatch[i-1], RECORDSIZE, 1, Fout);
    Namesubcatch[i-1] = fgets(&tmp, IDsubcatch[i-1]+1, Fout);
    }
  
  // --- extract node names
  std::vector<int>  IDnodes(SWMM_Nnodes);
  std::vector<std::string> Namenodes(SWMM_Nnodes);
  for ( int i=1; i<=SWMM_Nnodes; ++i) 
  {
    fread(&IDnodes[i-1], RECORDSIZE, 1, Fout);
    Namenodes[i-1] = fgets(&tmp, IDnodes[i-1]+1, Fout); 
  }

  // --- extract link names
  std::vector<int> IDlinks(SWMM_Nlinks);
  std::vector<std::string> Namelinks(SWMM_Nlinks);
  for ( int i=1; i<=SWMM_Nlinks; ++i) 
  {
    fread(&IDlinks[i-1], RECORDSIZE, 1, Fout);
    Namelinks[i-1] = fgets(&tmp, IDlinks[i-1]+1, Fout); 
  }

  // --- extract pollutant names
  std::vector<int> IDpolls(SWMM_Npolluts);
  std::vector<std::string> Namepolls(SWMM_Npolluts);
  for ( int i=1; i<=SWMM_Npolluts; ++i) 
  {
    fread(&IDpolls[i-1], RECORDSIZE, 1, Fout);
    Namepolls[i-1] = fgets(&tmp, IDpolls[i-1]+1, Fout); 
  }

  // Skip over saved subcatch/node/link input values
  offset = (SWMM_Nsubcatch+2) * RECORDSIZE  // Subcatchment area
    + (3*SWMM_Nnodes+4) * RECORDSIZE  // Node type, invert & max depth
    + (5*SWMM_Nlinks+6) * RECORDSIZE; // Link type, z1, z2, max depth & length
    offset = offset0 + offset;
    fseek(Fout, offset, SEEK_SET);
    
    // Read number & codes of computed variables
    fread(&SubcatchVars, RECORDSIZE, 1, Fout); // # Subcatch variables
    fseek(Fout, SubcatchVars*RECORDSIZE, SEEK_CUR);
    fread(&NodeVars, RECORDSIZE, 1, Fout);     // # Node variables
    fseek(Fout, NodeVars*RECORDSIZE, SEEK_CUR);
    fread(&LinkVars, RECORDSIZE, 1, Fout);     // # Link variables
    fseek(Fout, LinkVars*RECORDSIZE, SEEK_CUR);
    fread(&SysVars, RECORDSIZE, 1, Fout);     // # System variables
    
    // --- read data just before start of output results
    offset = StartPos - 3*RECORDSIZE;
    fseek(Fout, offset, SEEK_SET);
    fread(&SWMM_StartDate, sizeof(double), 1, Fout);
    fread(&SWMM_ReportStep, RECORDSIZE, 1, Fout);
    
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

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::NumericVector GetSwmmResult(int iType, int iIndex, int vIndex)
//-----------------------------------------------------------------------------
{
  int offset;
  std::vector<float> resultvec(SWMM_Nperiods);
  
  // --- compute offset into output file

  for ( int i=1; i<=SWMM_Nperiods; ++i)
  {
    offset = StartPos + (i-1)*BytesPerPeriod + 2*RECORDSIZE;
    if ( iType == SUBCATCH )
    {
      offset += RECORDSIZE*(iIndex*SubcatchVars + vIndex);
    }
    else if (iType == NODE)
    {
      offset += RECORDSIZE*(SWMM_Nsubcatch*SubcatchVars +
        iIndex*NodeVars + vIndex);
    }
    else if (iType == LINK)
    {
      offset += RECORDSIZE*(SWMM_Nsubcatch*SubcatchVars +
        SWMM_Nnodes*NodeVars +
        iIndex*LinkVars + vIndex);
    }
    else if (iType == SYS)
    {
      offset += RECORDSIZE*(SWMM_Nsubcatch*SubcatchVars +
        SWMM_Nnodes*NodeVars +
        SWMM_Nlinks*LinkVars + vIndex);
    }
    else return wrap(resultvec);
    
    // --- re-position the file and read the result
    fseek(Fout, offset, SEEK_SET);
    
    fread(&resultvec[i-1], RECORDSIZE, 1, Fout);
  }

  return wrap(resultvec);
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
  if (Fout != NULL)
  {
    fclose(Fout);
    Fout = NULL;
    return 1;
  }
  return 0;
}
