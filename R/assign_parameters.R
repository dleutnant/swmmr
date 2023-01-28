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
  UseMethod("assign_parameters")
}

#' conversion helper
#' @keywords internal
assign_parameters.default <- function(x, ...)
{
  tibble::as_tibble(x)
}

#' helper function: Transpose tibble of options
#' @keywords internal
assign_option_value <- function(x)
{
  rbind(Option = colnames(x), Value = x[1L, ]) %>% 
    t() %>% 
    tibble::as_tibble()
}

#' conversion helper
#' @keywords internal
assign_parameters.options <- function(x, ...)
{
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.report <- function(x, ...)
{
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.evaporation <- function(x, ...)
{
  assign_option_value(x)
}

#' conversion helper
#' @keywords internal
assign_parameters.subcatchments <- function(x, ...)
{
  defaults <- get_column_defaults()$subcatchments
  
  if (!all(names(defaults) %in% colnames(x))) {
 
    typologies <- get_from_args("subcatchment_typologies", ...)
    
    x <- if (given(typologies)) {
      # Take parameters defined in subcatchment_typologies
      dplyr::full_join(x, typologies, by = "Type")
    } else {
      # Take default values for missing columns
      add_columns_if_missing(x, defaults)
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
assign_parameters.subareas <- function(x, ...)
{
  x$Subcatchment <- x$Name
  
  defaults <- get_column_defaults()$subareas
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    typologies <- get_from_args("subcatchment_typologies", ...)
    
    x <- if (given(typologies)) {
      # Take values defined in subcatchment_typologies
      dplyr::full_join(x, typologies, by = "Type")
    } else {
      # Take default values for missing columns
      add_columns_if_missing(x, defaults)
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
assign_parameters.polygons <- function(x, ...)
{
  y <- sf::st_coordinates(x$geometry) %>% 
    .[, c(1, 2, ncol(.))] %>% 
    tibble::as_tibble(.)
  
  colnames(y) <- c("X", "Y", "index")
  
  x$index <- seq_along(x$Name)

  merge(x[, c("index", "Name")], y, by = "index") %>% 
    .[, c("Name", "X", "Y")]
}

#' conversion helper
#' @keywords internal
assign_parameters.infiltration <- function(x, ...)
{
  # separate model and subcatchment
  model <- x[[1]]
  x <- x[[2]]

  # Provide defaults for infiltration-related columns (if applicable, else NULL)
  defaults <- if (model %in% c("Horton", "Green_Ampt")) {
    
    element <- ifelse(
      model == "Horton", 
      "infiltration_horton", 
      "infiltration_green_ampt"
    )

    get_column_defaults()[[element]]
    
  } # else NULL implicitly    
  
  add_defaults_or_skip <- function(x, defaults) {
    if (given(defaults)) {
      # Add default columns and select infiltration columns
      add_columns_if_missing(x, defaults)[, c('Subcatchment', names(defaults))]
    } else {
      x
    }
  }
  
  infiltration <- get_from_args("infiltration", ...)
  
  if (given(infiltration)) {
    
    x$Subcatchment <- x$Name
    
    # Take values defined in infiltration
    x <- dplyr::full_join(x, infiltration, by = "Soil")
    
    # Fill missing columns with default and select infiltration columns
    x <- add_defaults_or_skip(x, defaults)
    
  } else {
    
    if (given(defaults)) {
      
      x$Subcatchment <- x$Name
      
      # Fill missing columns with default and select infiltration columns
      x <- add_defaults_or_skip(x, defaults)
    }
  }
  
  x
}

#' conversion helper
#' @keywords internal
assign_parameters.coverages <- function(x, ...)
{
  subcatchment <- get_from_args("subcatchment", ...)
  
  if (given(subcatchment)) {
    
    if ("Type" %in% colnames(subcatchment)) {
      
      x <- dplyr::full_join(subcatchment, x, by = c("Type" = "SurfaceType"))
      x <- x[!is.na(x$LandUse), c("Name", "LandUse", "PercentCoverage")]

    } else {

      clean_warning("Cannot use 'subcatchment' data. Missing column: 'Type'")
    }
  } 
  
  x
}

#' conversion helper
#' @keywords internal
assign_parameters.junctions <- function(x, ...)
{
  if ("Top" %in% colnames(x)) {
    # Calculate maximum depth
    x$Ymax <- x$Top - x$Bottom
  }
  
  # Set invert elevation
  x$Elevation <- x$Bottom
  
  defaults <- get_column_defaults()$junction
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    parameters <- get_from_args("junction_parameters", ...)
    
    x <- if (given(parameters)) {
      
      # Merge with values defined in junction_parameters
      dplyr::full_join(x, parameters, by = "Name")
      
    } else {
      
      # Take default values
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
assign_parameters.coordinates <- function(x, ...)
{
  y <- sf::st_coordinates(x$geometry) %>% 
    tibble::as_tibble(.)
  
  x$index <- seq_along(x$Name)
  y$index <- seq_along(y$X)
  
  merge(x[, c("index", "Name")], y, by = "index") %>% 
    .[, c("Name", "X", "Y")]
}

#' conversion helper
#' @keywords internal
assign_parameters.outfalls <- function(x, ...)
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
assign_parameters.conduits <- function(x, ...)
{
  defaults <- get_column_defaults()$conduits
  
  if (!all(names(defaults) %in% colnames(x))) {
    
    conduit_material <- get_from_args("conduit_material", ...)
    
    x <- if (given(conduit_material)) {
      
      # Take values given in conduit_material
      dplyr::full_join(conduit_material, x, by = "Material")
      
    } else {
      
      # Take default value
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
assign_parameters.xsections <- function(x, ...)
{
  # Rename Name of conduit to Link
  x$Link <- x$Name
  
  defaults <- get_column_defaults()$xsections
  
  # Default is circular shape
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
assign_parameters.pumps <- function(x, ...)
{
  # Add default values?
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
assign_parameters.weirs <- function(x, ...)
{
  # Add default values?
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
assign_parameters.storage <- function(x, ...)
{
  # Add default values ?
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
assign_parameters.curves <- function(x, ...)
{
  # Delete duplicated type descriptions
  is_duplicated <- duplicated(x[, c("Name", "Type")])
  
  x[["Type"]][is_duplicated] <- " "
  
  x
}
