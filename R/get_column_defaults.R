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
    )
  )
}
