#' conversion helper
#' @keywords internal
assign_parameters <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  UseMethod("assign_parameters", x)
}

#' conversion helper
#' @keywords internal
assign_parameters.default <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  tibble::as_tibble(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.options <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  # transpose tibble of options
  rbind(Report_Option = colnames(x), Values = x[1, ]) %>% 
    t(.) %>% 
    tibble::as_tibble(.)
}

#' conversion helper
#' @keywords internal
assign_parameters.report <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  # transpose tibble of report
  rbind(Option = colnames(x), Values = x[1, ]) %>% 
    t(.) %>% 
    tibble::as_tibble(.)
}

#' conversion helper
#' @keywords internal
assign_parameters.evaporation <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  # transpose tibble of evaporation
  rbind(Option = colnames(x), Values = x[1, ]) %>% 
    t(.) %>% 
    tibble::as_tibble(.)
}

#' conversion helper
#' @keywords internal
assign_parameters.subcatchments <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies, 
  conduit_material = NULL, junction_parameters = NULL
) {

  columns <- colnames(x)
  
  if (
    !("Rain_Gage" %in% columns) | 
    !("CurbLen" %in% columns) | 
    !("Snowpack" %in% columns) | 
    !("PercImperv" %in% columns) | 
    !("Slope" %in% columns) | 
    !("Width" %in% columns)
  ) {
    
    if (is.null(subcatchment_typologies) == F) {
      
      #...take parameters defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #... take default values for missing columns
      if (!("Rain_Gage" %in% columns)) {
        x$Rain_Gage <- "default"
      }
      
      if (!("CurbLen" %in% columns)) {
        x$CurbLen <- 0
      }
      
      if (!("Snowpack" %in% columns)) {
        x$Snowpack <- ' '
      }
      
      if (!("PercImperv" %in% columns)) {
        x$PercImperv <- 25
      }
      
      if (!("Slope" %in% columns)) {
        x$Slope <- 0.5
      }
      
      if (!("Width" %in% columns)) {
        x$Width <- 500
      }
    }
  }

  x[, c("Name", "Rain_Gage", "Outlet", "Area", "PercImperv", "Width", "Slope", 
        "CurbLen", "Snowpack")]
}

#' conversion helper
#' @keywords internal
assign_parameters.subareas <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  x$Subcatchment <- x$Name
  
  columns <- colnames(x)
  
  if(
    !("N_Imperv" %in% columns)| 
    !("N_Perv" %in% columns) | 
    !("S_Imperv" %in% columns) | 
    !("S_Perv" %in% columns) | 
    !("Pct_Zero" %in% columns) | 
    !("RouteTo" %in% columns) | 
    !("PctRouted" %in% columns)
  ) {
    
    if (is.null(subcatchment_typologies) == F) {
      
      #...take values defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #...take default values for missing columns
      if (!("N_Imperv" %in% columns)) {
        x$N_Imperv <- 0.01
      }
      
      if (!("N_Perv" %in% columns)) {
        x$N_Perv <- 0.1
      }
      
      if (!("S_Imperv" %in% columns)) {
        x$S_Imperv <- 0.05
      }
      
      if (!("S_Perv" %in% columns)) {
        x$S_Perv <- 0.05
      }
      
      if (!("Pct_Zero" %in% columns)) {
        x$Pct_Zero <- 25
      }
      
      if (!("RouteTo" %in% columns)) {
        x$RouteTo <- "OUTLET"
      }
      
      if (!("PctRouted" %in% columns)) {
        x$PctRouted <- 100
      }
    }
  }
  
  x[, c("Subcatchment", "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero",
        "RouteTo", "PctRouted")]
}

#' conversion helper
#' @keywords internal
assign_parameters.polygons <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  y <- sf::st_coordinates(x$geometry) %>% 
    .[, c(1, 2, ncol(.))] %>% 
    tibble::as_tibble(.)
  
  colnames(y) <- c("X", "Y", "index")
  
  x$index <- 1:length(x$Name)
  z <- x[, c("index", "Name")]
  
  merge(z, y, by.x = "index", by.y = "index") %>% 
    .[, c("Name", "X", "Y")]
}

#' conversion helper
#' @keywords internal
assign_parameters.infiltration <- function(
  x, infiltration, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  # separate model and subcatchment
  model <- x[[1]]
  x <- x[[2]]
  
  if (is.null(infiltration) == F) {
    
    x$Subcatchment <- x$Name
    #...take values defined in infiltration
    x <- dplyr::full_join(x, infiltration, by = "Soil")
    
    columns <- colnames(x)
    
    #... fill missing columns with default and select infiltration columns
    if (model == "Horton") {
      
      #...take default values
      if (!('MaxRate' %in% columns)) {
        x$MaxRate <- 3 
      }
      
      if (!('MinRate' %in% columns)) {
        x$MinRate <- 0.5 
      }
      
      if (!('Decay' %in% columns)) {
        x$Decay <- 4
      }
      
      if (!('DryTime' %in% columns)) {
        x$DryTime <- 7
      }
      
      if (!('MaxInfl' %in% columns)) {
        x$MaxInfl <- 0
      }
      
      #... select infiltration columns
      x <- x[, c('Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 
                 'MaxInfl')]
    }
    
    if (model == "Green_Ampt") {
      
      #...take default values
      if (!('Suction' %in% columns)) {
        x$Suction <- 3 
      }
      
      if (!('HydCon' %in% columns)) {
        x$HydCon <- 0.5 
      }
      
      if (!('IMDMax' %in% columns)) {
        x$IMDMax <- 4
      }
      
      #... select infiltration columns
      x <- x[, c('Subcatchment', 'Suction', 'HydCon', 'IMDMax')]
    }
    
  } else {
    
    columns <- colnames(x)
    
    if (model == "Horton") {
      
        x$Subcatchment <- x$Name
    
        #...take default values
        if (!('MaxRate' %in% columns)) {
          x$MaxRate <- 3 
        }
        
        if (!('MinRate' %in% columns)) {
          x$MinRate <- 0.5 
        }
        
        if (!('Decay' %in% columns)) {
          x$Decay <- 4
        }
        
        if (!('DryTime' %in% columns)) {
          x$DryTime <- 7
        }
        
        if (!('MaxInfl' %in% columns)) {
          x$MaxInfl <- 0
        }
        
        x <- x[, c( 'Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 
                    'MaxInfl')]
    }
    
    if (model == "Green_Ampt") {
      x$Subcatchment <- x$Name
      
      #...take default values
      if (!('Suction' %in% columns)) {
        x$Suction <- 3 
      }
      
      if (!('HydCon' %in% columns)) {
        x$HydCon <- 0.5 
      }
      
      if (!('IMDMax' %in% columns)) {
        x$IMDMax <- 4
      }
	  x <- x[, c('Subcatchment', 'Suction', 'HydCon', 'IMDMax')]
    }
  }

  x
}

#' conversion helper
#' @keywords internal
assign_parameters.coverages <- function(
  x, infiltration = NULL, subcatchment, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  if (is.null(subcatchment)) {
    
    if ("Type" %in% colnames(subcatchment)) {
      
      x <- dplyr::full_join(subcatchment, x, by = c("Type" = "SurfaceType"))
      x <- x[is.na(x$LandUse) == F, c("Name", "LandUse", "PercentCoverage")]
      
      return(x) 
    }
  } else{
    x
  }
}

#' conversion helper
#' @keywords internal
assign_parameters.junction <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters
) {
  
  columns <- colnames(x)
  
  if ("Top" %in% columns) {
    #... calculate maximum depth
    x$Ymax <- x$Top-x$Bottom
  }
  
  #... set invert elevation
  x$Elevation <- x$Bottom
  
  if (
    !("!('Y' %in% colnames(x))" %in% columns) | 
    !("!('Ysur' %in% colnames(x))" %in% columns) | 
    !("Apond" %in% columns)
  ) {
    
    if (is.null(junction_parameters) == F) {
      #...merge with values defined in junction_parameters
      x <- dplyr::full_join(x, junction_parameters, by = "Name")
    } else {
      #...take default values
     x$Y <- 0
     x$Ysur <- 0
     x$Apond <- 0
    }
  }
  
  x[, c("Name", "Elevation", "Ymax", "Y", "Ysur", "Apond")]
}

#' conversion helper
#' @keywords internal
assign_parameters.coordinates <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  y <- sf::st_coordinates(x$geometry) %>% 
    tibble::as_tibble(.)
  
  x$index <- 1:length(x$Name)
  y$index <- 1:length(y$X)
  
  z <- x[, c("index", "Name")]
  
  merge(z, y, by.x = "index", by.y = "index") %>% .[, c("Name", "X", "Y")]
}

#' conversion helper
#' @keywords internal
assign_parameters.outfalls <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {

  x$Elevation <- x$Bottom
  
  columns <- colnames(x)
  
  if (!("Gated" %in% columns)) {
    x$Gated <- "NO"
  }
  
  if (!("StageData" %in% columns)) {
    x$StageData <- ' '
  }
  
  if (!("RouteTo" %in% columns)) {
    x$RouteTo <- ' '
  }
  
  x[, c("Name", "Elevation", "Type", "StageData", "Gated", "RouteTo")]
}

#' conversion helper
#' @keywords internal
assign_parameters.conduits <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material, junction_parameters = NULL
) {
  
  if (!("Roughness" %in% colnames(x))) {
    
    if (is.null(conduit_material) == F) {
      #... take values given in conduit_material
      x <- dplyr::full_join(conduit_material, x, by = "Material")
    } else {
      #...take default value
      x$Roughness = 0.018
    }
  }
  
  x[, c("Name", "FromNode", "ToNode", "Length", "Roughness", "InOffset", 
        "OutOffset")]
}

#' conversion helper
#' @keywords internal
assign_parameters.xsections <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  #... rename Name of conduit to Link
  x$Link <- x$Name
  
  columns <- colnames(x)
  
  #... default is circular shape
  if (!("Shape" %in% columns)){
    x$Shape <- "CIRCULAR"
  }
  
  if (!("Geom1" %in% columns)){
    x$Geom1 <- 3
  }
  
  if (!("Geom2" %in% columns)){
    x$Geom2 <- 0
  }
  
  if (!("Geom3" %in% columns)){
    x$Geom3 <- 0
  }
  
  if (!("Geom4" %in% columns)){
    x$Geom4 <- 0
  }
  
  if (!("Barrels" %in% columns)){
    x$Barrels <- 1
  }
  
  x[, c("Link", "Shape", "Geom1", "Geom2", "Geom3", "Geom4", "Barrels")]
}

#' conversion helper
#' @keywords internal
assign_parameters.pumps <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  #...add default values ?
  x[, c("Name", "FromNode", "ToNode", "Pcurve", "status", "Startup", "Shutoff")]
}

#' conversion helper
#' @keywords internal
assign_parameters.weirs <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  #...add default values ?
  x[, c("Name", "FromNode", "ToNode", "Type", "CrestHt", "Cd",  "Gated", "EC", 
        "Cd2", "Sur")]
}

#' conversion helper
#' @keywords internal
assign_parameters.storage <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  #...add default values ?
  x[, c("Name", "Elev", "Ymax", "Y0", "Shape", "Curve_Name", "N_A", "Fevap")]
}

#' conversion helper
#' @keywords internal
assign_parameters.curves <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  #... delete duplicated type descriptions
  x[duplicated(x$Type), 'Type'] <- ' '
  
  x
}
