#' Get List of Default Values for Columns for Different Objects
#' @keywords internal
get_column_defaults <- function()
{
  list(
    
    subcatchments = list(
      Rain_Gage = "default", 
      CurbLen = 0, 
      Snowpack = " ", 
      PercImperv = 25, 
      Slope = 0.5,
      Width = 500
    ),
    
    subareas =list(
      N_Imperv = 0.01, 
      N_Perv = 0.1, 
      S_Imperv = 0.05, 
      S_Perv = 0.05, 
      Pct_Zero = 25, 
      RouteTo = "OUTLET", 
      PctRouted = 100
    ),
    
    infiltration_horton = list(
      MaxRate = 3,
      MinRate = 0.5,
      Decay = 4,
      DryTime = 7,
      MaxInfl = 0
    ),
    
    infiltration_green_ampt = list(
      Suction = 3,
      HydCon = 0.5,
      IMDMax = 4
    ),
    
    junction = list(
      Y = 0, 
      Ysur = 0,
      Apond = 0
    ),
    
    outfalls = list(
      Gated = "NO",
      StageData = " ",
      RouteTo = " "
    ),
    
    conduits = list(
      Roughness = 0.018
    ),
    
    xsections = list(
      Shape = "CIRCULAR",
      Geom1 = 3,
      Geom2 = 0,
      Geom3 = 0,
      Geom4 = 0,
      Barrels = 1
    ),
    
    options = list(
      FLOW_UNITS = "CMS",
      INFILTRATION = "HORTON", # capital letters!
      FLOW_ROUTING = "KINWAVE",
      LINK_OFFSETS = "DEPTH",
      FORCE_MAIN_EQUATION = "H-W",
      IGNORE_RAINFALL = "NO",
      IGNORE_SNOWMELT = "YES",
      IGNORE_GROUNDWATER = "YES",
      IGNORE_RDII = "YES",
      IGNORE_ROUTING = "YES",
      IGNORE_QUALITY = "YES",
      ALLOW_PONDING = "NO",
      SKIP_STEADY_STATE = "NO",
      SYS_FLOW_TOL = "5",
      LAT_FLOW_TOL = "5",
      START_DATE = "1/1/2017",
      START_TIME = "0:00:00",
      END_DATE = "1/1/2017",
      END_TIME = "1:00:00",
      REPORT_START_DATE = "1/1/2017",
      REPORT_START_TIME = "1:00:00",
      SWEEP_START = "1/1",
      SWEEP_END = "12/31",
      DRY_DAYS = "14",
      REPORT_STEP = "0:15:00",
      WET_STEP = "0:05:00",
      DRY_STEP = "1:00:00",
      ROUTING_STEP = "0:00:05",
      LENGTHENING_STEP = "0",
      VARIABLE_STEP = "0",
      MINIMUM_STEP = "0.5",
      INERTIAL_DAMPING = "NONE",
      NORMAL_FLOW_LIMITED = "BOTH",
      MIN_SURFAREA = "0",
      MIN_SLOPE = "0",
      MAX_TRIALS = "8",
      HEAD_TOLERANCE = "0.0015",
      THREADS = "1",
      TEMPDIR = " "
    ),
    
    report = list(
      INPUT = "NO",
      CONTROLS = "NO",
      SUBCATCHMENTS = "ALL",
      NODES = "ALL",
      LINKS = "ALL"
    ),
    
    evaporation = list(
      CONSTANT = 0,
      DRY_ONLY = 0
    )
  )
}
