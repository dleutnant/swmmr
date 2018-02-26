#' conversion helper
#' @keywords internal
assign_parameters <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  UseMethod("assign_parameters", x)
}

#' conversion helper
#' @keywords internal
assign_parameters.default <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){ 
 tibble::as_tibble(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.options <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  # transpose tibble of options
  x <- rbind(Report_Option = colnames(x), Values = x[1,]) %>% t(.) %>% tibble::as_tibble(.)
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.report <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  # transpose tibble of report
  x <- rbind(Option = colnames(x), Values = x[1,]) %>% t(.) %>% tibble::as_tibble(.)
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.evaporation <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  # transpose tibble of evaporation
  x <- rbind(Option = colnames(x), Values = x[1,]) %>% t(.) %>% tibble::as_tibble(.)
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.subcatchments <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies, conduit_material = NULL, junction_parameters = NULL){
  
  if(!("Rain_Gage" %in% colnames(x)) | !("CurbLen" %in% colnames(x)) | !("Snowpack" %in% colnames(x)) | !("PercImperv" %in% colnames(x)) | !("Slope" %in% colnames(x)) | !("Width" %in% colnames(x))){
    if(is.null(subcatchment_typologies)==F){
      #...take parameters defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by="Type")
    }else{
      
      #... take default values for missing columns
      if(!("Rain_Gage" %in% colnames(x))){
        x$Rain_Gage <- "default"
      }
      if(!("CurbLen" %in% colnames(x))){
        x$CurbLen <- 0
      }
      if(!("Snowpack" %in% colnames(x))){
        x$Snowpack <- ' '
      }
      if(!("PercImperv" %in% colnames(x))){
        x$PercImperv <- 25
      }
      if(!("Slope" %in% colnames(x))){
        x$Slope <- 0.5
      }
      if(!("Width" %in% colnames(x))){
        x$Width <- 500
      }
    }
  }

  x <- x[, c("Name", "Rain_Gage", "Outlet","Area", "PercImperv", "Width", "Slope", "CurbLen", "Snowpack")]
  
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.subareas <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies, conduit_material = NULL, junction_parameters = NULL){
  
  x$Subcatchment <- x$Name
  
  if(!("N_Imperv" %in% colnames(x))| !("N_Perv" %in% colnames(x)) | !("S_Imperv" %in% colnames(x)) | !("S_Perv" %in% colnames(x)) | !("Pct_Zero" %in% colnames(x)) | !("RouteTo" %in% colnames(x)) | !("PctRouted" %in% colnames(x))){
    if(is.null(subcatchment_typologies)==F){
      #...take values defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by="Type")
    }else{
      #...take default values for missing columns
      if(!("N_Imperv" %in% colnames(x))){
        x$N_Imperv <- 0.01
      }
      if(!("N_Perv" %in% colnames(x))){
        x$N_Perv <- 0.1
      }
      if(!("S_Imperv" %in% colnames(x))){
        x$S_Imperv <- 0.05
      }
      if(!("S_Perv" %in% colnames(x))){
        x$S_Perv <- 0.05
      }
      if(!("Pct_Zero" %in% colnames(x))){
        x$Pct_Zero <- 25
      }
      if(!("RouteTo" %in% colnames(x))){
        x$RouteTo <- "OUTLET"
      }
      if(!("PctRouted" %in% colnames(x))){
        x$PctRouted <- 100
      }
    }
  }
  
  x <- x[,c("Subcatchment", "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", "PctRouted")]
  
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.polygons <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  
  y <- sf::st_coordinates(x$geometry) %>% .[, c(1,2,ncol(.))] %>% tibble::as_tibble(.)
  colnames(y) <- c("X","Y","index")
  
  x$index <- 1:length(x$Name)
  z <- x[,c("index", "Name")]
  x <- merge(z, y, by.x="index", by.y="index") %>% .[,c("Name", "X", "Y")]
  
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.infiltration <- function(x, infiltration, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  
  # separate model and subcatchment
  model <- x[[1]]
  x <- x[[2]]
  
    if(is.null(infiltration)==F){
    x$Subcatchment <- x$Name
    #...take values defined in infiltration
    x <- dplyr::full_join(x, infiltration, by="Soil")
    
    
    #... fill missing columns with default and select infiltration columns
    if(model == "Horton"){
      
      #...take default values
      if(!('MaxRate' %in% colnames(x))){
        x$MaxRate <- 3 
      }
      if(!('MinRate' %in% colnames(x))){
        x$MinRate <- 0.5 
      }
      if(!('Decay' %in% colnames(x))){
        x$Decay <- 4
      }
      if(!('DryTime' %in% colnames(x))){
        x$DryTime <- 7
      }
      if(!('MaxInfl' %in% colnames(x))){
        x$MaxInfl <- 0
      }
      
      #... select infiltration columns
      x <- x[,c('Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 'MaxInfl')]
    }
    
    if(model == "Green_Ampt"){
      
      #...take default values
      if(!('Suction' %in% colnames(x))){
        x$Suction <- 3 
      }
      if(!('HydCon' %in% colnames(x))){
        x$HydCon <- 0.5 
      }
      if(!('IMDMax' %in% colnames(x))){
        x$IMDMax <- 4
      }
      
      #... select infiltration columns
      x <- x[,c('Subcatchment', 'Suction', 'HydCon', 'IMDMax')]
    }
    
  }else{
    if(model == "Horton"){
        x$Subcatchment <- x$Name
    
        #...take default values
        if(!('MaxRate' %in% colnames(x))){
          x$MaxRate <- 3 
        }
        if(!('MinRate' %in% colnames(x))){
          x$MinRate <- 0.5 
        }
        if(!('Decay' %in% colnames(x))){
          x$Decay <- 4
        }
        if(!('DryTime' %in% colnames(x))){
          x$DryTime <- 7
        }
        if(!('MaxInfl' %in% colnames(x))){
          x$MaxInfl <- 0
        }
        x <- x[,c('Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 'MaxInfl')]
    }
    if(model == "Green_Ampt"){
      x$Subcatchment <- x$Name
      
      #...take default values
      if(!('Suction' %in% colnames(x))){
        x$Suction <- 3 
      }
      if(!('HydCon' %in% colnames(x))){
        x$HydCon <- 0.5 
      }
      if(!('IMDMax' %in% colnames(x))){
        x$IMDMax <- 4
      }
	  x <- x[,c('Subcatchment', 'Suction', 'HydCon', 'IMDMax')]
    }

  }

  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.coverages <- function(x, infiltration=NULL, subcatchment, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  if(is.null(subcatchment)){
    if("Type" %in% colnames(subcatchment)){
      x <- dplyr::full_join(subcatchment, x, by=c("Type" = "SurfaceType"))
      x <- x[is.na(x$LandUse)==F, c("Name", "LandUse", "PercentCoverage")]
      return(x) 
    }
  }else{
    x
  }
}

#' conversion helper
#' @keywords internal
assign_parameters.junction <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters){
  if("Top" %in% colnames(x)){
    #... calculate maximum depth
    x$Ymax <- x$Top-x$Bottom
  }
  
  #... set invert elevation
  x$Elevation <- x$Bottom
  
  if(!("!('Y' %in% colnames(x))" %in% colnames(x))| !("!('Ysur' %in% colnames(x))" %in% colnames(x)) | !("Apond" %in% colnames(x))){
    if(is.null(junction_parameters)==F){
      #...merge with values defined in junction_parameters
      x <- dplyr::full_join(x, junction_parameters, by="Name")
    }else{
      #...take default values
     x$Y <- 0
     x$Ysur <- 0
     x$Apond <- 0
    }
  }
  x <- x[,c("Name", "Elevation", "Ymax", "Y", "Ysur", "Apond")]
  return(x) 
}

#' conversion helper
#' @keywords internal
assign_parameters.coordinates <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  y <- sf::st_coordinates(x$geometry) %>% tibble::as_tibble(.)
  
  x$index <- 1:length(x$Name)
  y$index <- 1:length(y$X)
  z <- x[,c("index", "Name")]
  x <- merge(z, y, by.x="index", by.y="index") %>% .[,c("Name", "X", "Y")]
  
  return(x)
  
}

#' conversion helper
#' @keywords internal
assign_parameters.outfalls <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  x$Elevation <- x$Bottom
  
  if(!("Gated" %in% colnames(x))){
    x$Gated <- "NO"
  }
  
  if(!("StageData" %in% colnames(x))){
    x$StageData <- ' '
  }
  
  if(!("RouteTo" %in% colnames(x))){
    x$RouteTo <- ' '
  }
  
  x[,c("Name", "Elevation", "Type", "StageData", "Gated", "RouteTo")]
}


#' conversion helper
#' @keywords internal
assign_parameters.conduits <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material, junction_parameters = NULL){
  if(!("Roughness" %in% colnames(x))){
    if(is.null(conduit_material)==F){
      #... take values given in conduit_material
      x <- dplyr::full_join(conduit_material, x, by="Material")
    }else{
      #...take default value
      x$Roughness = 0.018
    }
   
  }
  x <- x[,c("Name", "FromNode", "ToNode", "Length", "Roughness", "InOffset", "OutOffset")]
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.xsections <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  #... rename Name of conduit to Link
  x$Link <- x$Name
  
  #... default is circular shape
  if(!("Shape" %in% colnames(x))){
      x$Shape <- "CIRCULAR"
  }
  if(!("Geom1" %in% colnames(x))){
      x$Geom1 <- 3
  }
  if(!("Geom2" %in% colnames(x))){
      x$Geom2 <- 0
  }
  if(!("Geom3" %in% colnames(x))){
      x$Geom3 <- 0
  }
  if(!("Geom4" %in% colnames(x))){
      x$Geom4 <- 0
  }
  if(!("Barrels" %in% colnames(x))){
      x$Barrels <- 1
  }

  x <- x[,c("Link", "Shape", "Geom1", "Geom2", "Geom3", "Geom4", "Barrels")]
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.pumps <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  #...add default values ?
  x <- x[,c("Name", "FromNode", "ToNode", "Pcurve", "status", "Startup", "Shutoff")]
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.weirs <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  #...add default values ?
  x <- x[,c("Name", "FromNode", "ToNode", "Type","CrestHt", "Cd",  "Gated", "EC", "Cd2", "Sur")]
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.storage <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  #...add default values ?
  x <- x[,c("Name", "Elev", "Ymax", "Y0", "Shape", "Curve_Name", "N_A", "Fevap")]
  return(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.curves <- function(x, infiltration=NULL, subcatchment=NULL, subcatchment_typologies=NULL, conduit_material = NULL, junction_parameters = NULL){
  #... delete duplicated type descriptions
  x[duplicated(x$Type),'Type'] <- ' '
  return(x)
}