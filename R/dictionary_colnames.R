
#' import helper
#' @keywords internal
compare_to_dictionary <- function(shp = NULL, sf = NULL) {

  # ...complete dictionary with all column names given in swmmr:

  dictionary_complete <- tibble::tibble(
    org_swmm = c(
      "Name", "Rain_Gage", "Outlet", "Area", "Perc_Imperv", "Width", 
      "Perc_Slope", "CurbLen", "Snowpack", "N-Imperv", "N-Perv", "S-Imperv","S-Perv","PctZero","RouteTo","PctRouted","MaxRate","MinRate","Decay","DryTime","MaxInfil","Name","Elevation","MaxDepth","InitDepth","SurDepth","Aponded",  "Name","Elevation","Type","Stage Data","Gated","Route To", "Name","From_Node","To_Node","Length","Roughness","InOffset","OutOffset","InitFlow","MaxFlow", "Shape","Geom1","Geom2","Geom3","Geom4","Barrels","Culvert", "Name","From Node","To Node","Pump Curve","Status","Sartup","Shutoff", "Name","From_Node","To_Node"   ,"Type","CrestHt","Qcoeff","Gated","EndCon","EndCoeff","Surcharge","RoadWidth","RoadSurf", "Name", "Elev.", "MaxDepth", "InitDepth", "Shape_1", "Curve Name/Params", "N/A", "Fevap", "Psi", "Ksat", "IMD"), 
    shp_abb = c("Name","RainGag","Outlet","Area","Prc_Imp","Width","Prc_Slp","CurbLen","Snowpck","N.Imprv","N.Perv","S.Imprv","S.Perv","PctZero","RouteTo","PctRotd","MaxRate","MinRate","Decay","DryTime","MaxInfl","Name","Elevation","MaxDepth","InitDepth","SurDepth","Aponded","Name","Elevation","Type", "Stage.Data", "Gated", "RouteTo", "Name","From.Node","To.Node","Length","Roughness","InOffset","OutOffset","InitFlow","MaxFlow","Shape" ,"Geom1","Geom2","Geom3","Geom4","Barrels", "Culvert", "Name","From.Node","To.Node","Pump.Curve","Status","Sartup","Shutoff", "Name","From.Node","To.Node" ,"Type","CrestHt","Qcoeff","Gated","EndCon","EndCoeff","Surcharge","RoadWidth", "RoadSurf", "Name", "Elev_", "MaxDpth", "IntDpth", "Shape", "CrvNm.P", "N.A", "Fevap", "Psi", "Ksat", "IMD"),
    int_shp_to_inp = c("Name", "Rain_Gage", "Outlet","Area", "PercImperv", "Width", "Slope", "CurbLen", "Snowpack", "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", "PctRouted", "MaxRate", "MinRate", "Decay", "DryTime", "MaxInfl", "Name", "Bottom", "Ymax", "Y", "Ysur", "Apond", "Name", "Bottom", "Type", "StageData", "Gated", "RouteTo", "Name", "FromNode", "ToNode", "Length", "Roughness", "InOffset", "OutOffset", "InitFlow", "MaxFlow", "Shape", "Geom1","Geom2","Geom3","Geom4","Barrels", "Culvert", "Name", "FromNode", "ToNode", "Pcurve", "status", "Startup", "Shutoff", "Name", "FromNode", "ToNode", "Type","CrestHt", "Cd",  "Gated", "EC", "Cd2", "Sur", "RoadWidth", "RoadSurf","Name", "Elev", "Ymax", "Y0", "Shape", "Curve_Name", "N_A", "Fevap", "Psi", "Ksat", "IMD"), 
    section = c(rep("subcatchment",9), rep("subarea", 7), rep("infiltration_Horton", 5), rep("junction",6), rep("outfalls", 6), rep("conduit",9),rep("xsection", 7), rep("pump", 7), rep("weir",12), rep("storage",11))
  )

  # writeLines(paste0('"', kwb.utils::pasteColumns(sep = ",", as.data.frame(do.call(rbind,lapply(1:nrow(dictionary_complete), function(i) {
  #   c(
  #     dictionary_complete$section[i],
  #     dictionary_complete$org_swmm[i],
  #     dictionary_complete$shp_abb[i],
  #     dictionary_complete$int_shp_to_inp[i]
  #   )
  # })))), '", '))
  
  text <- c(
    "section,org_swmm,shp_abb,int_shp_to_inp",
    "subcatchment,Name,Name,Name", 
    "subcatchment,Rain_Gage,RainGag,Rain_Gage", 
    "subcatchment,Outlet,Outlet,Outlet", 
    "subcatchment,Area,Area,Area", 
    "subcatchment,Perc_Imperv,Prc_Imp,PercImperv", 
    "subcatchment,Width,Width,Width", 
    "subcatchment,Perc_Slope,Prc_Slp,Slope", 
    "subcatchment,CurbLen,CurbLen,CurbLen", 
    "subcatchment,Snowpack,Snowpck,Snowpack", 
    "subarea,N-Imperv,N.Imprv,N_Imperv", 
    "subarea,N-Perv,N.Perv,N_Perv", 
    "subarea,S-Imperv,S.Imprv,S_Imperv", 
    "subarea,S-Perv,S.Perv,S_Perv", 
    "subarea,PctZero,PctZero,Pct_Zero", 
    "subarea,RouteTo,RouteTo,RouteTo", 
    "subarea,PctRouted,PctRotd,PctRouted", 
    "infiltration_Horton,MaxRate,MaxRate,MaxRate", 
    "infiltration_Horton,MinRate,MinRate,MinRate", 
    "infiltration_Horton,Decay,Decay,Decay", 
    "infiltration_Horton,DryTime,DryTime,DryTime", 
    "infiltration_Horton,MaxInfil,MaxInfl,MaxInfl", 
    "junction,Name,Name,Name", 
    "junction,Elevation,Elevation,Bottom", 
    "junction,MaxDepth,MaxDepth,Ymax", 
    "junction,InitDepth,InitDepth,Y", 
    "junction,SurDepth,SurDepth,Ysur", 
    "junction,Aponded,Aponded,Apond", 
    "outfalls,Name,Name,Name", 
    "outfalls,Elevation,Elevation,Bottom", 
    "outfalls,Type,Type,Type", 
    "outfalls,Stage Data,Stage.Data,StageData", 
    "outfalls,Gated,Gated,Gated", 
    "outfalls,Route To,RouteTo,RouteTo", 
    "conduit,Name,Name,Name", 
    "conduit,From_Node,From.Node,FromNode", 
    "conduit,To_Node,To.Node,ToNode", 
    "conduit,Length,Length,Length", 
    "conduit,Roughness,Roughness,Roughness", 
    "conduit,InOffset,InOffset,InOffset", 
    "conduit,OutOffset,OutOffset,OutOffset", 
    "conduit,InitFlow,InitFlow,InitFlow", 
    "conduit,MaxFlow,MaxFlow,MaxFlow", 
    "xsection,Shape,Shape,Shape", 
    "xsection,Geom1,Geom1,Geom1", 
    "xsection,Geom2,Geom2,Geom2", 
    "xsection,Geom3,Geom3,Geom3", 
    "xsection,Geom4,Geom4,Geom4", 
    "xsection,Barrels,Barrels,Barrels", 
    "xsection,Culvert,Culvert,Culvert", 
    "pump,Name,Name,Name", 
    "pump,From Node,From.Node,FromNode", 
    "pump,To Node,To.Node,ToNode", 
    "pump,Pump Curve,Pump.Curve,Pcurve", 
    "pump,Status,Status,status", 
    "pump,Sartup,Sartup,Startup", 
    "pump,Shutoff,Shutoff,Shutoff", 
    "weir,Name,Name,Name", 
    "weir,From_Node,From.Node,FromNode", 
    "weir,To_Node,To.Node,ToNode", 
    "weir,Type,Type,Type", 
    "weir,CrestHt,CrestHt,CrestHt", 
    "weir,Qcoeff,Qcoeff,Cd", 
    "weir,Gated,Gated,Gated", 
    "weir,EndCon,EndCon,EC", 
    "weir,EndCoeff,EndCoeff,Cd2", 
    "weir,Surcharge,Surcharge,Sur", 
    "weir,RoadWidth,RoadWidth,RoadWidth", 
    "weir,RoadSurf,RoadSurf,RoadSurf", 
    "storage,Name,Name,Name", 
    "storage,Elev.,Elev_,Elev", 
    "storage,MaxDepth,MaxDpth,Ymax", 
    "storage,InitDepth,IntDpth,Y0", 
    "storage,Shape_1,Shape,Shape", 
    "storage,Curve Name/Params,CrvNm.P,Curve_Name", 
    "storage,N/A,N.A,N_A", 
    "storage,Fevap,Fevap,Fevap", 
    "storage,Psi,Psi,Psi", 
    "storage,Ksat,Ksat,Ksat", 
    "storage,IMD,IMD,IMD"
  )
  
  dictionary_complete_2 <- tibble::as_tibble(read.csv(text = text)[, c(2,3,4,1)])
  
  identical(dictionary_complete, dictionary_complete_2)
  
  # -----
  if(!is.null(shp)){
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
  
  if(!is.null(sf)){
     # cut dictionary to column names original and internal in sf_to_inp that differ:
    dictionary <- dictionary_complete[-which(duplicated(dictionary_complete$org_swmm) == T), ]
    dictionary <- subset(dictionary, dictionary$org_swmm != dictionary$int_shp_to_inp)
  
    # set internal column names for original ones in sf-object:
    for (i in 1:length(colnames(sf))) {
      if (colnames(sf)[i] %in% dictionary$org_swmm) {
        row <- which(colnames(sf)[i] == dictionary$org_swmm)
        colnames(sf)[i] <- dictionary$int_shp_to_inp[row]
      }
    }

    return(sf)
  }
  
 
  
}