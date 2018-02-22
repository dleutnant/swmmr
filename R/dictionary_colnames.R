
#' import helper
#' @keywords internal
compare_to_dictionary <- function(shp) {

  # ...complete dictionary with all column names given in swmmr:

    dictionary_complete <- tibble::tibble(org_swmm = c("Name","Rain Gage","Outlet","Area","Perc_Imperv","Width","Perc_Slope","CurbLen","Snowpack","N-Imperv","N-Perv","S-Imperv","S-Perv","PctZero","RouteTo","PctRouted","MaxRate","MinRate","Decay","DryTime","MaxInfil","Name","Elevation","MaxDepth","InitDepth","SurDepth","Aponded",  "Name","Elevation","Type","Stage Data","Gated","Route To", "Name","From Node","To Node","Length","Roughness","InOffset","OutOffset","InitFlow","MaxFlow", "Shape","Geom1","Geom2","Geom3","Geom4","Barrels","Culvert", "Name","From Node","To Node","Pump Curve","Status","Sartup","Shutoff", "Name","From Node","To Node"   ,"Type","CrestHt","Qcoeff","Gated","EndCon","EndCoeff","Surcharge","RoadWidth","RoadSurf", "Name", "Elev.", "MaxDepth", "InitDepth", "Shape", "Curve Name/Params", "N/A", "Fevap", "Psi", "Ksat", "IMD"), 
                         shp_abb = c("Name","RainGag","Outlet","Area","Prc_Imp","Width","Prc_Slp","CurbLen","Snowpck","N.Imprv","N.Perv","S.Imprv","S.Perv","PctZero","RouteTo","PctRotd","MaxRate","MinRate","Decay","DryTime","MaxInfl","Name","Elevation","MaxDepth","InitDepth","SurDepth","Aponded","Name","Elevation","Type", "Stage.Data", "Gated", "RouteTo", "Name","From.Node","To.Node","Length","Roughness","InOffset","OutOffset","InitFlow","MaxFlow","Shape" ,"Geom1","Geom2","Geom3","Geom4","Barrels", "Culvert", "Name","From.Node","To.Node","Pump.Curve","Status","Sartup","Shutoff", "Name","From.Node","To.Node" ,"Type","CrestHt","Qcoeff","Gated","EndCon","EndCoeff","Surcharge","RoadWidth", "RoadSurf", "Name", "Elev_", "MaxDpth", "IntDpth", "Shape", "CrvNm.P", "N.A", "Fevap", "Psi", "Ksat", "IMD"),
                         int_shp_to_inp = c("Name", "Rain_Gage", "Outlet","Area", "PercImperv", "Width", "Slope", "CurbLen", "Snowpack", "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", "PctRouted", "MaxRate", "MinRate", "Decay", "DryTime", "MaxInfl", "Name", "Bottom", "Ymax", "Y", "Ysur", "Apond", "Name", "Bottom", "Type", "StageData", "Gated", "RouteTo", "Name", "FromNode", "ToNode", "Length", "Roughness", "InOffset", "OutOffset", "InitFlow", "MaxFlow", "Shape", "Geom1","Geom2","Geom3","Geom4","Barrels", "Culvert", "Name", "FromNode", "ToNode", "Pcurve", "status", "Startup", "Shutoff", "Name", "FromNode", "ToNode", "Type","CrestHt", "Cd",  "Gated", "EC", "Cd2", "Sur", "RoadWidth", "RoadSurf","Name", "Elev", "Ymax", "Y0", "Shape", "Curve_Name", "N_A", "Fevap", "Psi", "Ksat", "IMD"), 
                         section = c(rep("subcatchment",9), rep("subarea", 7), rep("infiltration_Horton", 5), rep("junction",6), rep("outfalls", 6), rep("conduit",9),rep("xsection", 7), rep("pump", 7), rep("weir",12), rep("storage",11))
						 )

  # -----

  # cut dictionary to column names abbreviated and internal in shp_to_inp that differ:
  dictionary <- dictionary_complete[-which(duplicated(dictionary_complete$shp_abb) == T), ]
  dictionary <- subset(dictionary, dictionary$shp_abb != dictionary$int_shp_to_inp)

  # ----

  # set internal column names for abbreviated ones in shp-file

  for (i in 1:length(colnames(shp))) {
    if (colnames(shp)[i] %in% dictionary$shp_abb) {
      row <- which(colnames(shp)[i] == dictionary$shp_abb)
      colnames(shp)[i] <- dictionary$int_shp_to_inp[row]
    }
  }

  return(shp)
}