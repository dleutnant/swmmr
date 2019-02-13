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

  main_columns <- c(
    "Rain_Gage", "CurbLen", "Snowpack", "PercImperv", "Slope", "Width"
  )
  
  if (! all(main_columns %in% colnames(x))) {
    
    if (is.null(subcatchment_typologies) == F) {
      
      #...take parameters defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #... take default values for missing columns
      x <- add_column_if_missing(x, "Rain_Gage", "default")
      x <- add_column_if_missing(x, "CurbLen", 0)
      x <- add_column_if_missing(x, "Snowpack", ' ')
      x <- add_column_if_missing(x, "PercImperv", 25)
      x <- add_column_if_missing(x, "Slope", 0.5)
      x <- add_column_if_missing(x, "Width", 500)
    }
  }

  x[, c("Name", "Rain_Gage", "Outlet", "Area", "PercImperv", "Width", "Slope", 
        "CurbLen", "Snowpack")]
}

#' Add Column With Default Value if Not in Data Frame
#' @keywords internal
add_column_if_missing <- function(df, column, default) {
  
  if (! column %in% colnames(df)) {
    df[[column]] <- default
  }
  
  df
}

#' conversion helper
#' @keywords internal
assign_parameters.subareas <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  x$Subcatchment <- x$Name
  
  main_columns <- c(
    "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", 
    "PctRouted"
  )
  
  if (! all(main_columns %in% colnames(x))) {
    
    if (is.null(subcatchment_typologies) == F) {
      
      #...take values defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #...take default values for missing columns
      x <- add_column_if_missing(x, "N_Imperv", 0.01)
      x <- add_column_if_missing(x, "N_Perv", 0.1)
      x <- add_column_if_missing(x, "S_Imperv", 0.05)
      x <- add_column_if_missing(x, "S_Perv", 0.05)
      x <- add_column_if_missing(x, "Pct_Zero", 25)
      x <- add_column_if_missing(x, "RouteTo", "OUTLET")
      x <- add_column_if_missing(x, "PctRouted", 100)
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
    
    #... fill missing columns with default and select infiltration columns
    if (model == "Horton") {
      
      #...take default values
      x <- add_column_if_missing(x, 'MaxRate', 3)
      x <- add_column_if_missing(x, 'MinRate', 0.5)
      x <- add_column_if_missing(x, 'Decay', 4)
      x <- add_column_if_missing(x, 'DryTime', 7)
      x <- add_column_if_missing(x, 'MaxInfl', 0)
      
      #... select infiltration columns
      x <- x[, c('Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 
                 'MaxInfl')]
    }
    
    if (model == "Green_Ampt") {
      
      #...take default values
      x <- add_column_if_missing(x, 'Suction', 3)
      x <- add_column_if_missing(x, 'HydCon', 0.5)
      x <- add_column_if_missing(x, 'IMDMax', 4)
      
      #... select infiltration columns
      x <- x[, c('Subcatchment', 'Suction', 'HydCon', 'IMDMax')]
    }
    
  } else {

    if (model == "Horton") {
      
        x$Subcatchment <- x$Name
    
        #...take default values
        x <- add_column_if_missing(x, 'MaxRate', 3)
        x <- add_column_if_missing(x, 'MinRate', 0.5)
        x <- add_column_if_missing(x, 'Decay', 4)
        x <- add_column_if_missing(x, 'DryTime', 7)
        x <- add_column_if_missing(x, 'MaxInfl', 0)
        
        x <- x[, c( 'Subcatchment', 'MaxRate', 'MinRate', 'Decay', 'DryTime', 
                    'MaxInfl')]
    }
    
    if (model == "Green_Ampt") {
      
      x$Subcatchment <- x$Name
      
      #...take default values
      x <- add_column_if_missing(x, 'Suction', 3)
      x <- add_column_if_missing(x, 'HydCon', 0.5)
      x <- add_column_if_missing(x, 'IMDMax', 4)
      
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
  
  if ("Top" %in% colnames(x)) {
    #... calculate maximum depth
    x$Ymax <- x$Top - x$Bottom
  }
  
  #... set invert elevation
  x$Elevation <- x$Bottom
  
  main_columns <- c(
    "!('Y' %in% colnames(x))",    # this is an error, isn't it? 
    "!('Ysur' %in% colnames(x))", # this is an error, isn't it?
    "Apond"
  )
  
  if (! all(main_columns %in% colnames(x))) {
    
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
  
  x <- add_column_if_missing(x, "Gated", "NO")
  x <- add_column_if_missing(x, "StageData", ' ')
  x <- add_column_if_missing(x, "RouteTo", ' ')
  
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
  
  #... default is circular shape
  x <- add_column_if_missing(x, "Shape", "CIRCULAR")
  x <- add_column_if_missing(x, "Geom1", 3)
  x <- add_column_if_missing(x, "Geom2", 0)
  x <- add_column_if_missing(x, "Geom3", 0)
  x <- add_column_if_missing(x, "Geom4", 0)
  x <- add_column_if_missing(x, "Barrels", 1)
  
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
