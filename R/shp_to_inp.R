#' Convert *.shp files to SWMM's *.inp file
#'
#' Reads *.shp files and other information needed for SWMM's *.inp file and
#' returns a list of class inp. If paths are not specified default values are
#' taken.
#'
#' @param path_options Name (incl. path) to a .txt file with SWMM sections.
#'   Write section name in lower case and in squared brackets. The following
#'   sections are allowed: options, report, raingages, evaporation, pollutant,
#'   landuse, buildup, washoff, coverages.
#' @param path_polygon Name (incl. path) to a .shp file with polygons features.
#'   At least the following subcatchment related columns must be specified:
#'   Name, Outlet, Area, RouteTo.
#' @param subcatchment_typologies R data.frame or tibble with further
#'   subcatchment related parameters. If subcatchment_typologies is given,
#'   polygon feature has to include a column named Type. Parameters defined in
#'   subcatchment_typologies parameters are merged to subcatchments by Type.
#' @param path_point Name (incl. path) to a .shp file with point features. At
#'   least the following junction related columns must be specified: Name,
#'   Bottom and Top or Ymax.
#' @param junction_parameters R data.frame or tibble with further junction
#'   related parameters (e.g. Surcharge depth).
#' @param path_outfall Name (incl. path) to a .shp file with point features. At
#'   least the following outfall related columns must be specified: Name,
#'   Bottom, Type, Gated.
#' @param path_line Name (incl. path) to a .shp file with line features. At
#'   least the following conduit related coulumns must be specified: Name,
#'   Length, Type, FromNode, ToNode, OutOffset, Geom1.
#' @param conduit_material R data.frame or tibble with further conduit related
#'   parameters (e.g. roughness). If conduit_material is given, line feature has
#'   to include a column named Material. Parameters defined in conduit_material
#'   parameters are merged to conduits by Material.
#' @param path_timeseries Name (incl. path) to a .dat file with a timeseries in
#'   SWMM format.
#' @param infiltration R data.frame or tibble with infiltration parameters
#'   related to soil properties. If infiltration is given, polygon feature has
#'   to include a column named soil. Infiltration parameters are merged to
#'   subcatchments by soil name.
#' @param path_pumps Name (incl. path) to a .shp file with line features. All
#'   parameters must be given: Name, FromNode, ToNode, Pcurve, status, Startup,
#'   Shutoff.
#' @param path_pump_curve Name (incl. path) to a .txt file with pump curve
#'   information. Having the following structure: "Name of pump" "PUMP1-4" "x"
#'   "y", without header.
#' @param path_weirs Name (incl. path) to a .shp file with line features. All
#'   parameters must be given: Name, FromNode, ToNode, Type, CrestHt, Cd, Gated,
#'   EC, Cd2, Sur.
#' @param path_storage Name (incl. path) to a .shp file with storage curve
#'   information. The following parameters must be given: Name, Elev, Ymax, Y0,
#'   Shape, Curve_Name, N_A, Fevap.
#' @param path_storage_curve Name (incl. path) to a .txt file with storage curve
#'   information. Having the following structure: "Name of storage unit"
#'   "Storage" "x" "y", without header.
#' @return A list of class inp.
#' @export
#' @rdname shp_to_inp
shp_to_inp <- function(
    path_options = NULL, 
    path_polygon = NULL, 
    subcatchment_typologies = NULL, 
    path_point = NULL, 
    junction_parameters = NULL, 
    path_outfall = NULL, 
    path_line = NULL, 
    conduit_material = NULL, 
    path_timeseries = NULL, 
    infiltration = NULL, 
    path_pumps = NULL, 
    path_pump_curve = NULL, 
    path_weirs = NULL, 
    path_storage = NULL, 
    path_storage_curve = NULL
) 
{
  # helper function
  read_and_normalise <- function(path, quiet = TRUE) {
    path %>%
      sf::st_read(stringsAsFactors = FALSE, quiet = quiet) %>% 
      tibble::as_tibble() %>% 
      compare_to_dictionary(shp = .)
  }

  # read spatial data:
  
  # check if polygon shape is available, return error message or read shape:
  subcatchment <- if (is.null(path_polygon)) {
    clean_warning(
      "Define path to polygon file including filename and ending (or object ", 
      "polygon_sf alternatively) otherwise sections subcatchments, subareas, ", 
      "infiltration are missing."
    )
    # specify object subcatchment which is called when calling:
    # assign_parameters.coverages
    NULL
  } else {
    read_and_normalise(path_polygon)
  }
  
  # ... and for the junction point shape:
  junctions <- if (is.null(path_point)) {
    clean_warning(
      "Define path to point file including filename and ending (or object ", 
      "point_sf alternatively) otherwise sections junctions and coordinates ", 
      "are missing."
    )
    # specify object junction which is called when testing column names for warn
    # message in junction_parameters
    NULL
    
  } else {
    read_and_normalise(path_point)
  }
  
  # ... also do it for the outfall point shape:
  outfalls <- if (is.null(path_outfall)) {
    clean_warning(
      "Define path to outfall file including filename and ending (or object ", 
      "outfall_sf alternatively) otherwise section outfall is missing."
    )
    NULL
  } else {
    read_and_normalise(path_outfall)
  }
  
  # ...add Pumps section if pumps_sf exists
  pumps <- if (is.null(path_pumps)) {
    NULL
  } else {
    read_and_normalise(path_pumps)
  }
  
  # ...add weirs if path_weirs or weirs_sf exists
  weirs <- if (is.null(path_weirs)) {
    NULL
  } else {
    read_and_normalise(path_weirs)
  }
  
  # ...add storages if path_storage or storage_sf exists
  storage <- if (is.null(path_storage)) {
    NULL
  } else{
    read_and_normalise(path_storage)
  }
  
  # ...do the same for the conduit line shape:
  conduits <- if (is.null(path_line)) {
    clean_warning(
      "Define path to line file including filename and ending (or line_sf ", 
      "alternatively) otherwise section conduits is missing."
    )
    # specify object conduits which is called when testing column names for warn
    # message in conduit_material
    NULL
  } else {
    read_and_normalise(path_line)
  }
  
  # read supplementary data and check data for completeness, return a
  # list_of_sections:
  list_of_sections <- input_to_list_of_sections(
    path_options, 
    subcatchment,
    subcatchment_typologies, 
    junctions,
    junction_parameters, 
    outfalls,
    conduits,
    conduit_material, 
    path_timeseries, 
    infiltration, 
    pumps, 
    path_pump_curve, 
    weirs,
    storage,
    path_storage_curve
  )
  
  # process data stores in list_of_sections and return an object of class inp:
  list_of_sections_to_inp(
    list_of_sections, 
    infiltration, 
    subcatchment_typologies, 
    conduit_material, 
    junction_parameters
  )
}
