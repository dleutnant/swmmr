#' Convert sf objects to SWMM's *.inp file
#'
#' Reads sf objects and other information needed for SWMM's *.inp file and
#' returns a list of class inp. If paths are not specified default values are
#' taken.
#'
#' @param path_options Name (incl. path) to a .txt file with SWMM sections.
#'   Write section name in lower case and in squared brackets. The following
#'   sections are allowed: options, report, raingages, evaporation, pollutant,
#'   landuse, buildup, washoff, coverages.
#' @param polygon_sf A sf object with polygons features. At least the following
#'   subcatchment related columns must be specified: Name, Outlet, Area,
#'   RouteTo.
#' @param subcatchment_typologies R data.frame or tibble with further
#'   subcatchment related parameters. If subcatchment_typologies is given,
#'   polygon feature has to include a column named Type. Parameters defined in
#'   subcatchment_typologies parameters are merged to subcatchments by Type.
#' @param point_sf A sf object with point features. At least the following
#'   junction related columns must be specified: Name, Bottom and Top or Ymax.
#' @param junction_parameters R data.frame or tibble with further junction
#'   related parameters (e.g. Surcharge depth).
#' @param outfall_sf A sf object with point features. At least the following
#'   outfall related columns must be specified: Name, Bottom, Type, Gated.
#' @param line_sf A sf object with line features. At least the following conduit
#'   related coulumns must be specified: Name, Length, Type, FromNode, ToNode,
#'   OutOffset, Geom1.
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
#' @param pumps_sf A sf object with line features. All parameters must be given:
#'   Name, FromNode, ToNode, Pcurve, status, Startup, Shutoff.
#' @param path_pump_curve Name (incl. path) to a .txt file with pump curve
#'   information. Having the following structure: "Name of pump" "PUMP1-4" "x"
#'   "y", without header.
#' @param weirs_sf Alternative to path_weirs: A sf object with line features.
#'   All parameters must be given: Name, FromNode, ToNode, Type, CrestHt, Cd,
#'   Gated, EC, Cd2, Sur.
#' @param storage_sf A sf object with point features including storage point
#'   information. The following parameters must be given: Name, Elev, Ymax, Y0,
#'   Shape, Curve_Name, N_A, Fevap.
#' @param path_storage_curve Name (incl. path) to a .txt file with storage curve
#'   information. Having the following structure: "Name of storage unit"
#'   "Storage" "x" "y", without header.
#' @return A list of class inp.
#' @export
#' @rdname sf_to_inp
sf_to_inp <- function(
    path_options = NULL, 
    polygon_sf = NULL,
    subcatchment_typologies = NULL, 
    point_sf = NULL,
    junction_parameters = NULL, 
    outfall_sf = NULL,
    line_sf = NULL,
    conduit_material = NULL, 
    path_timeseries = NULL, 
    infiltration = NULL, 
    pumps_sf = NULL, 
    path_pump_curve = NULL, 
    weirs_sf = NULL,
    storage_sf = NULL,
    path_storage_curve = NULL
)
{
  # Helper functions
  as_normalised_tibble <- function(x) {
    tibble::as_tibble(x) %>%
      rename_columns_using_dict(from = "org_swmm")
  }
  
  # Read spatial data
  
  # Check if polygon shape is available, return error message or read shape
  subcatchment <- if (given(polygon_sf)) {
    as_normalised_tibble(polygon_sf)
  } else {
    clean_warning(
      "Define path to polygon file including filename and ending (or object ", 
      "polygon_sf alternatively) otherwise sections subcatchments, subareas, ", 
      "infiltration are missing."
    )
    # specify object subcatchment which is called when calling:
    # assign_parameters.coverages
  }
  
  # And for the junction point shape
  junctions <- if (given(point_sf)) {
    as_normalised_tibble(point_sf)
  } else {
    clean_warning(
      "Define path to point file including filename and ending (or object ", 
      "point_sf alternatively) otherwise sections junctions and coordinates ", 
      "are missing."
    )
    # specify object junction which is called when testing column names for warn
    # message in junction_parameters
  }
  
  # Also do it for the outfall point shape
  outfalls <- if (given(outfall_sf)) {
    as_normalised_tibble(outfall_sf)
  } else {
    clean_warning(
      "Define path to outfall file including filename and ending (or object ", 
      "outfall_sf alternatively) otherwise section outfall is missing."
    )
  }
  
  # Add Pumps section if pumps_sf exists, otherwise pumps is NULL
  pumps <- if (given(pumps_sf)) {
    as_normalised_tibble(pumps_sf)
  }
  
  # Add weirs if path_weirs or weirs_sf exists, otherwise weirs is NULL
  weirs <- if (given(weirs_sf)) {
    as_normalised_tibble(weirs_sf)
  }
  
  # Add storages if path_storage or storage_sf exists, otherwise storage is NULL
  storage <- if (given(storage_sf)) {
    as_normalised_tibble(storage_sf)
  }
  
  # Do the same for the conduit line shape
  conduits <- if (given(line_sf)) {
    as_normalised_tibble(line_sf)
  } else {
    clean_warning(
      "Define path to line file including filename and ending (or line_sf ", 
      "alternatively) otherwise section conduits is missing."
    )
    # specify object conduits which is called when testing column names for warn
    # message in conduit_material
  }
  
  # Read supplementary data and check data for completeness, return a list of 
  # sections
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
  
  # Process data stores in list_of_sections and return an object of class inp
  list_of_sections_to_inp(
    list_of_sections, 
    infiltration, 
    subcatchment_typologies, 
    conduit_material, 
    junction_parameters
  )
}
