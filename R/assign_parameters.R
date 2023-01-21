#' conversion helper
#' @keywords internal
assign_parameters <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  UseMethod("assign_parameters", x)
}

#' conversion helper
#' @keywords internal
assign_parameters.default <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  tibble::as_tibble(x)
}

#' helper function
#' @keywords internal
assign_option_value <- function(x)
{
  # transpose tibble of options
  rbind(Option = colnames(x), Value = x[1L, ]) %>% 
    t() %>% 
    tibble::as_tibble()
}

#' conversion helper
#' @keywords internal
assign_parameters.options <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.report <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  # transpose tibble of report
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.evaporation <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.subcatchments <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  defaults <- get_column_defaults()$subcatchments
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    x <- if (is.null(subcatchment_typologies)) {
      #... take default values for missing columns
      add_columns_if_missing(x, defaults)
    } else {
      #...take parameters defined in subcatchment_typologies
      dplyr::full_join(x, subcatchment_typologies, by = "Type")
    }
  }
  
  columns <- c(
    "Name", "Rain_Gage", "Outlet", "Area", "PercImperv", "Width", "Slope", 
    "CurbLen", "Snowpack"
  )
  
  # Lookup column names in dictionary
  d <- get_column_dictionary()
  columns2 <- d$int_shp_to_inp[d$section == "subcatchment"]
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.subareas <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  x$Subcatchment <- x$Name
  
  defaults <- get_column_defaults()$subareas
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    x <- if (is.null(subcatchment_typologies)) {
      #...take default values for missing columns
      add_columns_if_missing(x, defaults)
    } else {
      #...take values defined in subcatchment_typologies
      dplyr::full_join(x, subcatchment_typologies, by = "Type")
    }
  }
  
  columns <- c(
    "Subcatchment", "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero",
    "RouteTo", "PctRouted"
  )
  
  # Lookup column names in dictionary
  d <- get_column_dictionary()
  columns2 <- c("Subcatchment", d$int_shp_to_inp[d$section == "subarea"])
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.polygons <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
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
    x, 
    infiltration, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  # separate model and subcatchment
  model <- x[[1]]
  x <- x[[2]]

  # Provide defaults for infiltration-related columns (if applicable, else NULL)
  infiltration_defaults <- if (model %in% c("Horton", "Green_Ampt")) {
    
    element <- ifelse(
      model == "Horton", 
      "infiltration_horton", 
      "infiltration_green_ampt"
    )

    get_column_defaults()[[element]]
    
  } # else NULL implicitly    
  
  add_defaults_or_skip <- function(x, defaults) {
    if (is.null(defaults)) {
      return(x)
    }
    # Add default columns and select infiltration columns
    add_columns_if_missing(x, defaults)[, c('Subcatchment', names(defaults))]
  }
  
  if (! is.null(infiltration)) {
    
    x$Subcatchment <- x$Name
    
    #...take values defined in infiltration
    x <- dplyr::full_join(x, infiltration, by = "Soil")
    
    #... fill missing columns with default and select infiltration columns
    x <- add_defaults_or_skip(x, infiltration_defaults)
    
  } else {
    
    if (!is.null(infiltration_defaults)) {
      
      x$Subcatchment <- x$Name
      
      #... fill missing columns with default and select infiltration columns
      x <- add_defaults_or_skip(x, infiltration_defaults)
    }
  }
  
  x
}

#' conversion helper
#' @keywords internal
assign_parameters.coverages <- function(
    x, 
    infiltration = NULL, 
    subcatchment, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  if (is.null(subcatchment)) {
    
    if ("Type" %in% colnames(subcatchment)) {
      
      x <- dplyr::full_join(subcatchment, x, by = c("Type" = "SurfaceType"))
      x <- x[! is.na(x$LandUse), c("Name", "LandUse", "PercentCoverage")]
      
      return(x) 
    }
    
  } else {
    
    x
  }
}

#' conversion helper
#' @keywords internal
assign_parameters.junctions <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters
)
{
  if ("Top" %in% colnames(x)) {
    #... calculate maximum depth
    x$Ymax <- x$Top - x$Bottom
  }
  
  #... set invert elevation
  x$Elevation <- x$Bottom
  
  defaults <- get_column_defaults()$junction
  
  if (! all(names(defaults) %in% colnames(x))) {
    
    x <- if (! is.null(junction_parameters)) {
      
      #...merge with values defined in junction_parameters
      dplyr::full_join(x, junction_parameters, by = "Name")
      
    } else {
      
      #...take default values
      add_columns_if_missing(x, defaults, force = TRUE)
    }
  }
  
  columns <- c("Name", "Elevation", "Ymax", names(defaults))
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- replace_values(
    d$int_shp_to_inp[d$section == "junction"],
    from = "Bottom",
    to = "Elevation"
  )
  stopifnot(identical(columns, columns2))  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.coordinates <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  y <- sf::st_coordinates(x$geometry) %>% 
    tibble::as_tibble(.)
  
  x$index <- seq_along(x$Name)
  y$index <- seq_along(y$X)
  
  z <- x[, c("index", "Name")]
  
  merge(z, y, by.x = "index", by.y = "index") %>% 
    .[, c("Name", "X", "Y")]
}

#' conversion helper
#' @keywords internal
assign_parameters.outfalls <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  x$Elevation <- x$Bottom
  
  defaults <- get_column_defaults()$outfalls
  
  x <- add_columns_if_missing(x, defaults)
  
  columns <- c("Name", "Elevation", "Type", "StageData", "Gated", "RouteTo")
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- replace_values(
    d$int_shp_to_inp[d$section == "outfalls"], 
    from = "Bottom", 
    to = "Elevation"
  )
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.conduits <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material, 
    junction_parameters = NULL
)
{
  defaults <- get_column_defaults()$conduits
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    x <- if (! is.null(conduit_material)) {
      
      #... take values given in conduit_material
      dplyr::full_join(conduit_material, x, by = "Material")
      
    } else {
      
      #...take default value
      add_columns_if_missing(x, defaults, force = TRUE)
    }
  }
  
  columns <- c(
    "Name", "FromNode", "ToNode", "Length", "Roughness", "InOffset", "OutOffset"
  )
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- setdiff(
    d$int_shp_to_inp[d$section == "conduit"], 
    c("InitFlow", "MaxFlow")
  )
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.xsections <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  #... rename Name of conduit to Link
  x$Link <- x$Name
  
  defaults <- get_column_defaults()$xsections
  
  #... default is circular shape
  x <- add_columns_if_missing(x, defaults)
  
  columns <- c("Link", names(defaults))
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- c(
    "Link", 
    setdiff(d$int_shp_to_inp[d$section == "xsection"], "Culvert")
  )
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.pumps <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  #...add default values ?
  columns <- c(
    "Name", "FromNode", "ToNode", "Pcurve", "status", "Startup", "Shutoff"
  )
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- d$int_shp_to_inp[d$section == "pump"]
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.weirs <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  #...add default values ?
  columns <- c(
    "Name", "FromNode", "ToNode", "Type", "CrestHt", "Cd",  "Gated", "EC", 
    "Cd2", "Sur"
  )
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- setdiff(
    d$int_shp_to_inp[d$section == "weir"],
    c("RoadWidth", "RoadSurf")
  )
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.storage <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  #...add default values ?
  columns <- c(
    "Name", "Elev", "Ymax", "Y0", "Shape", "Curve_Name", "N_A", "Fevap"
  )
  
  # Get column names from dictionary
  d <- get_column_dictionary()
  columns2 <- setdiff(
    d$int_shp_to_inp[d$section == "storage"], 
    c("Psi", "Ksat", "IMD")
  )
  stopifnot(identical(columns, columns2))
  
  x[, columns2]
}

#' conversion helper
#' @keywords internal
assign_parameters.curves <- function(
    x, 
    infiltration = NULL, 
    subcatchment = NULL, 
    subcatchment_typologies = NULL, 
    conduit_material = NULL, 
    junction_parameters = NULL
)
{
  #... delete duplicated type descriptions
  x[duplicated(x$Type), 'Type'] <- ' '
  
  x
}
