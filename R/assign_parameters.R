#' Get List of Default Values for Columns for Different Objects
#' @keywords internal
get_column_defaults <- function() {
  
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
  rbind(Option = colnames(x), Value = x[1, ]) %>% 
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
  rbind(Option = colnames(x), Value = x[1, ]) %>% 
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
  rbind(Option = colnames(x), Value = x[1, ]) %>% 
    t(.) %>% 
    tibble::as_tibble(.)
}

#' conversion helper
#' @keywords internal
assign_parameters.subcatchments <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies, 
  conduit_material = NULL, junction_parameters = NULL
) {

  defaults <- get_column_defaults()$subcatchments

  if (! all(names(defaults) %in% colnames(x))) {
    
    if (! is.null(subcatchment_typologies)) {
      
      #...take parameters defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #... take default values for missing columns
      x <- add_columns_if_missing(x, defaults)
    }
  }

  x[, c("Name", "Rain_Gage", "Outlet", "Area", "PercImperv", "Width", "Slope", 
        "CurbLen", "Snowpack")]
}

#' Add Columns With Default Values if Not in Data Frame
#' @keywords internal
add_columns_if_missing <- function(df, defaults, force = FALSE)
{
  for (column in names(defaults)) {
    df <- add_column_if_missing(df, column, defaults[[column]], force)
  }
  
  df
}
    
#' Add Column With Default Value if Not in Data Frame
#' @keywords internal
add_column_if_missing <- function(df, column, default, force = FALSE) {
  
  if (force || ! column %in% colnames(df)) {
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
  
  defaults <- get_column_defaults()$subareas

  if (! all(names(defaults) %in% colnames(x))) {
    
    if (! is.null(subcatchment_typologies)) {
      
      #...take values defined in subcatchment_typologies
      x <- dplyr::full_join(x, subcatchment_typologies, by = "Type")
      
    } else {
      
      #...take default values for missing columns
      x <- add_columns_if_missing(x, defaults)
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
  
  x$index <- seq_along(x$Name)
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
  
  if (! is.null(infiltration)) {
    
    x$Subcatchment <- x$Name
    #...take values defined in infiltration
    x <- dplyr::full_join(x, infiltration, by = "Soil")
    
    #... fill missing columns with default and select infiltration columns
    if (model == "Horton") {

      #...take default values
      defaults <- get_column_defaults()$infiltration_horton
      
      x <- add_columns_if_missing(x, defaults)

      #... select infiltration columns
      x <- x[, c('Subcatchment', names(defaults))]
    }
    
    if (model == "Green_Ampt") {
      
      #...take default values
      defaults <- get_column_defaults()$infiltration_green_ampt
      
      x <- add_columns_if_missing(x, defaults)

      #... select infiltration columns
      x <- x[, c('Subcatchment', names(defaults))]
    }
    
  } else {

    if (model == "Horton") {
      
        x$Subcatchment <- x$Name
    
        #...take default values
        defaults <- get_column_defaults()$infiltration_horton

        x <- add_columns_if_missing(x, defaults)

        x <- x[, c('Subcatchment', names(defaults))]
    }
    
    if (model == "Green_Ampt") {
      
      x$Subcatchment <- x$Name
      
      #...take default values
      defaults <- get_column_defaults()$infiltration_green_ampt

      x <- add_columns_if_missing(x, defaults)

      x <- x[, c('Subcatchment', names(defaults))]
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
      x <- x[! is.na(x$LandUse), c("Name", "LandUse", "PercentCoverage")]
      
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
  
  defaults <- get_column_defaults()$junction

  if (! all(names(defaults) %in% colnames(x))) {
    
    if (! is.null(junction_parameters)) {
      #...merge with values defined in junction_parameters
      x <- dplyr::full_join(x, junction_parameters, by = "Name")
    } else {
      #...take default values
      x <- add_columns_if_missing(x, defaults, force = TRUE)
    }
  }
  
  x[, c("Name", "Elevation", "Ymax", names(defaults))]
}

#' conversion helper
#' @keywords internal
assign_parameters.coordinates <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material = NULL, junction_parameters = NULL
) {
  
  y <- sf::st_coordinates(x$geometry) %>% 
    tibble::as_tibble(.)
  
  x$index <- seq_along(x$Name)
  y$index <- seq_along(y$X)
  
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

  defaults <- get_column_defaults()$outfalls

  x <- add_columns_if_missing(x, defaults)

  x[, c("Name", "Elevation", "Type", "StageData", "Gated", "RouteTo")]
}

#' conversion helper
#' @keywords internal
assign_parameters.conduits <- function(
  x, infiltration = NULL, subcatchment = NULL, subcatchment_typologies = NULL, 
  conduit_material, junction_parameters = NULL
) {

  defaults <- get_column_defaults()$conduits

  if (! all(names(defaults) %in% colnames(x))) {
    
    if (! is.null(conduit_material)) {
      #... take values given in conduit_material
      x <- dplyr::full_join(conduit_material, x, by = "Material")
    } else {
      #...take default value
      x <- add_columns_if_missing(x, defaults, force = TRUE)
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
  
  defaults <- get_column_defaults()$xsections

  #... default is circular shape
  x <- add_columns_if_missing(x, defaults)

  x[, c("Link", names(defaults))]
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
