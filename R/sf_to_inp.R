#' Convert sf objects to SWMM's *.inp file
#'
#' Reads sf objects and other information needed for SWMM's *.inp file and returns a list of class inp. If paths are not specified default values are taken.
#'
#' @param path_options Name (incl. path) to a .txt file with SWMM sections. Write section name in lower case and in squared brackets. The following sections are allowed: options, report, raingages, evaporation, pollutant, landuse, buildup, washoff, coverages.
#' @param polygon_sf A sf object with polygons features. At least the following subcatchment related columns must be specified: Name, Outlet, Area, RouteTo.
#' @param subcatchment_typologies R data.frame or tibble with further subcatchment related parameters. If subcatchment_typologies is given, polygon feature has to include a column named Type. Parameters defined in subcatchment_typologies parameters are merged to subcatchments by Type.
#' @param point_sf A sf object with point features. At least the following junction related columns must be specified: Name, Bottom and Top or Ymax.
#' @param junction_parameters R data.frame or tibble with further junction related parameters (e.g. Surcharge depth).
#' @param outfall_sf A sf object with point features. At least the following outfall related columns must be specified: Name, Bottom, Type, Gated.
#' @param line_sf A sf object with line features. At least the following conduit related coulumns must be specified: Name, Length, Type, FromNode, ToNode, OutOffset, Geom1.
#' @param conduit_material R data.frame or tibble with further conduit related parameters (e.g. roughness). If conduit_material is given, line feature has to include a column named Material. Parameters defined in conduit_material parameters are merged to conduits by Material.
#' @param path_timeseries Name (incl. path) to a .dat file with a timeseries in SWMM format.
#' @param infiltration R data.frame or tibble with infiltration parameters related to soil properties. If infiltration is given, polygon feature has to include a column named soil. Infiltration parameters are merged to subcatchments by soil name.
#' @param pumps_sf A sf object with line features. All parameters must be given: Name, FromNode, ToNode, Pcurve, status, Startup, Shutoff.
#' @param path_pump_curve Name (incl. path) to a .txt file with pump curve information. Having the following structure: "Name of pump" "PUMP1-4" "x" "y", without header.
#' @param weirs_sf Alternative to path_weirs: A sf object with line features. All parameters must be given: Name, FromNode, ToNode, Type, CrestHt, Cd, Gated, EC, Cd2, Sur.
#' @param storage_sf A sf object with point features including storage point information. The following parameters must be given: Name, Elev, Ymax, Y0, Shape, Curve_Name, N_A, Fevap.
#' @param path_storage_curve Name (incl. path) to a .txt file with storage curve information. Having the following structure: "Name of storage unit" "Storage" "x" "y", without header.
#' @return A list of class inp.
#' @export
#' @rdname sf_to_inp
sf_to_inp <- function(path_options = NULL, 
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
                       path_storage_curve = NULL) {
  
  # read spatial data:
  
  # check if polygon shape is available, return error message or read shape:
  if (is.null(polygon_sf)) {
    warning("Define path to polygon file including filename and ending (or object polygon_sf alternatively) otherwise sections subcatchments, subareas, infiltration are missing.")
    
    # specify object subcatchment which is called when calling: assign_parameters.coverages
    subcatchment <- NULL
    
  } else {
    # convert sf to tibble
    subcatchment <- tibble::as_tibble(polygon_sf) %>%
      compare_to_dictionary(sf = .)
  }
  
  # ... and for the junction point shape:
  if (is.null(point_sf)) {
    warning("Define path to point file including filename and ending (or object point_sf alternatively) otherwise sections junctions and coordinates are missing.")
    
    # specify object junction which is called when testing column names for warn message in junction_parameters
    junctions <- NULL
    
  } else {
    # convert sf to tibble
    junctions <- tibble::as_tibble(point_sf) %>%
      compare_to_dictionary(sf = .)
  }
  
  # ... also do it for the outfall point shape:
  if (is.null(outfall_sf)) {
    warning("Define path to outfall file including filename and ending (or object outfall_sf alternatively) otherwise section outfall is missing.")
    
  } else {
    # convert sf to tibble
    outfalls <- tibble::as_tibble(outfall_sf) %>%
      compare_to_dictionary(sf = .)
  }
    
  # ...add Pumps section if pumps_sf exists
  if(is.null(pumps_sf)){
    pumps <- NULL
  }else{
    # convert sf to tibble
    pumps <- tibble::as_tibble(pumps_sf) %>%
      compare_to_dictionary(sf = .)

  }
  
  # ...add weirs if path_weirs or weirs_sf exists
  if(is.null(weirs_sf)){
    weirs <- NULL
  }else{
    # convert sf to tibble
    weirs <- tibble::as_tibble(weirs_sf) %>%
      compare_to_dictionary(sf = .)
  }
  
  # ...add storages if path_storage or storage_sf exists
  if(is.null(storage_sf)){
    storage <- NULL
  }else{
    # convert sf to tibble
    storage <- tibble::as_tibble(storage_sf) %>%
      compare_to_dictionary(sf = .)
  }
  
  # ...do the same for the conduit line shape:
  if (is.null(line_sf)) {
    warning("Define path to line file including filename and ending (or line_sf alternatively) otherwise section conduits is missing.")
    
    # specify object conduits which is called when testing column names for warn message in conduit_material
    conduits <- NULL
  
  } else {
    # convert sf to tibble
    conduits <- tibble::as_tibble(line_sf) %>%
      compare_to_dictionary(sf = .)

  }

  # read supplementary data and check data for completeness, return a list_of_sections:
  list_of_sections <- input_to_list_of_sections(path_options, 
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
                                                path_storage_curve)
  
  # process data stores in list_of_sections and return an object of class inp:
  inp <- list_of_sections_to_inp(list_of_sections, 
                                 infiltration, 
                                 subcatchment_typologies, 
                                 conduit_material, 
                                 junction_parameters)
  
  return(inp)
    
}