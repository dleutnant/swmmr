#' Convert *.shp files to SWMM's *.inp file
#'
#' Reads *.shp files and other information needed for SWMM's *.inp file and returns a list of class inp. If paths are not specified default values are taken.
#'
#' @param path_options Name (incl. path) to a .txt file with SWMM sections. Write section name in lower case and in squared brackets. The following sections are allowed: options, report, raingages, evaporation, pollutant, landuse, buildup, washoff, coverages.
#' @param path_polygon Name (incl. path) to a .shp file with polygons features. At least the following subcatchment related columns must be specified: Name, Outlet, Area, RouteTo.
#' @param subcatchment_typologies R data.frame or tibble with further subcatchment related parameters. If subcatchment_typologies is given, polygon feature has to include a column named Type. Parameters defined in subcatchment_typologies parameters are merged to subcatchments by Type.
#' @param path_point Name (incl. path) to a .shp file with point features. At least the following junction related columns must be specified: Name, Bottom and Top or Ymax.
#' @param junction_parameters R data.frame or tibble with further junction related parameters (e.g. Surcharge depth).
#' @param path_outfall Name (incl. path) to a .shp file with point features. At least the following outfall related columns must be specified: Name, Bottom, Type, Gated.
#' @param path_line Name (incl. path) to a .shp file with line features. At least the following conduit related coulumns must be specified: Name, Length, Type, FromNode, ToNode, OutOffset, Geom1.
#' @param conduit_material R data.frame or tibble with further conduit related parameters (e.g. roughness). If conduit_material is given, line feature has to include a column named Material. Parameters defined in conduit_material parameters are merged to conduits by Material.
#' @param path_timeseries Name (incl. path) to a .dat file with a timeseries in SWMM format.
#' @param infiltration R data.frame or tibble with infiltration parameters related to soil properties. If infiltration is given, polygon feature has to include a column named soil. Infiltration parameters are merged to subcatchments by soil name.
#' @param path_pumps Name (incl. path) to a .shp file with line features. All parameters must be given: Name, FromNode, ToNode, Pcurve, status, Startup, Shutoff.
#' @param path_pump_curve Name (incl. path) to a .txt file with pump curve information. Having the following structure: "Name of pump" "PUMP1-4" "x" "y", without header.
#' @param path_weirs Name (incl. path) to a .shp file with line features. All parameters must be given: Name, FromNode, ToNode, Type, CrestHt, Cd, Gated, EC, Cd2, Sur.
#' @param path_storage Name (incl. path) to a .txt file with storage curve information. Having the following structure: "Name of storage" "Storage" "x" "y", without header.
#' @param path_storage_curve Name (incl. path) to a .txt file with storage curve information. Having the following structure: "Name of storage unit" "Storage" "x" "y", without header.
#' @return A list of class inp.
#' @export
#' @rdname shp_to_inp
shp_to_inp <- function(path_options = NULL, 
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
                       path_storage_curve = NULL) {

  # ... check missing arguments, add default or generate error messages, in some cases default values are added later...

  # check if options are available, otherwise add default:
  if (missing(path_options)) {
    warning("Options are missing, default values are taken.")
    list_of_sections <- list()

    # ... add default values for sections options, report, evaporation
    # options
    list_of_sections[["options"]] <- tibble::tibble(
      FLOW_UNITS = "CMS",
      INFILTRATION = "HORTON", # capital letters!
      FLOW_ROUTING = "KINWAVE",
      LINK_OFFSETS = "DEPTH",
      FORCE_MAIN_EQUATION = "H-W",
      IGNORE_RAINFALL = "NO",
      IGNORE_SNOWMELT = "YES",
      IGNORE_GROUNDWATER = "YES",
      IGNORE_RDII = "YES",
      IGNORE_ROUTING = "YES",
      IGNORE_QUALITY = "YES",
      ALLOW_PONDING = "NO",
      SKIP_STEADY_STATE = "NO",
      SYS_FLOW_TOL = "5",
      LAT_FLOW_TOL = "5",
      START_DATE = "1/1/2017",
      START_TIME = "0:00:00",
      END_DATE = "1/1/2017",
      END_TIME = "1:00:00",
      REPORT_START_DATE = "1/1/2017",
      REPORT_START_TIME = "1:00:00",
      SWEEP_START = "1/1",
      SWEEP_END = "12/31",
      DRY_DAYS = "14",
      REPORT_STEP = "0:15:00",
      WET_STEP = "0:05:00",
      DRY_STEP = "1:00:00",
      ROUTING_STEP = "0:00:05",
      LENGTHENING_STEP = "0",
      VARIABLE_STEP = "0",
      MINIMUM_STEP = "0.5",
      INERTIAL_DAMPING = "NONE",
      NORMAL_FLOW_LIMITED = "BOTH",
      MIN_SURFAREA = "0",
      MIN_SLOPE = "0",
      MAX_TRIALS = "8",
      HEAD_TOLERANCE = "0.0015",
      THREADS = "1",
      TEMPDIR = " "
    )

    list_of_sections[["report"]] <- tibble::tibble(
      INPUT = "NO",
      CONTROLS = "NO",
      SUBCATCHMENTS = "ALL",
      NODES = "ALL",
      LINKS = "ALL"
    )

    list_of_sections[["evaporation"]] <- tibble::tibble(
      CONSTANT = 0,
      DRY_ONLY = 0
    )
  } else {
    # ...use information from options (option.txt)
    options <- readLines(path_options)

    # ...delete empty lines
    options <- options[options != ""]

    # find section lines
    section_lines <- grep("\\[", options, value = F)
    section_end <- c(section_lines[-1] - 1, length(options))

    # separate section title
    section_title <- gsub(
      pattern = "\\[|\\]",
      replacement = "",
      x = options[section_lines]
    )

    # list all section blocks
    list_of_sections <- vector(length = length(section_lines), mode = "list")
    names(list_of_sections) <- section_title

    # make tibble and transpose it
    for (i in 1:length(section_lines)) {
      list_of_sections[[i]] <- options[(section_lines[i] + 1):(section_end[i])] %>%
        cbind(list_of_sections[[i]]) %>%
        tibble::as.tibble(.) %>%
        tidyr::separate(., 1, c("Variable", "Value"), "\t", extra = "merge", fill = "left")

      # define column vector to keep the order later
      cols <- list_of_sections[[i]][, 1] %>% unlist(.) %>% as.character(.)

      # transpose and keep order of original data
      list_of_sections[[i]] <- tidyr::gather(list_of_sections[[i]], Variable, Value) %>%
        tidyr::spread_(names(.)[1], "Value") %>%
        .[, cols]

      # separate rows with more than one entry (e.g. in pollution section)
      if (any(grepl("\t", list_of_sections[[i]]))) {
        list_of_sections[[i]] <- tidyr::separate_rows(list_of_sections[[i]], (1:ncol(list_of_sections[[i]])), sep = "\t")
      }
    }
  }

  # check if polygon shape is available, return error message or read shape and add subcatchment to sections:
  if (is.null(path_polygon)) {
    warning("Define path to polygon file including filename and ending otherwise sections subcatchments, subareas, infiltration are missing.")
    
    # specify object subcatchment which is called when calling: assign_parameters.coverages
    subcatchment <- NULL
    
  } else {
    # read the polygon file 
    subcatchment <- sf::st_read(path_polygon, stringsAsFactors = F, quiet = TRUE) %>% 
      tibble::as_tibble() %>% 
      compare_to_dictionary()
    
    # special case which should only occur if an inp has been exported using swmmr.
    # in this case Area_subcatchment is Ar_sbct and Area_LID_usage is Ar_ld_s.
    # Function was required to allow automatic tests with Example4.inp.
    if ("Ar_sbct" %in% colnames(subcatchment)) {
      colnames(subcatchment) <- gsub("Ar_sbct", "Area", colnames(subcatchment))
    }
    
    # check the structure of polygon file:
    if (all(c("Name","Outlet","Area","RouteTo") %in% colnames(subcatchment))) {
      list_of_sections[['subcatchments']]  <- subcatchment # subcatchment_typologies
      list_of_sections[['subareas']] <- subcatchment # subcatchment_typologies
      list_of_sections[['polygons']] <- subcatchment
    } else {
      stop("The polygon file has to include at least the columns named: Name, Outlet, Area, RouteTo. For optional column names ckeck the documentation.")
    }
    
    # check infiltration model
    if (list_of_sections$options$INFILTRATION == "Horton" | list_of_sections$options$INFILTRATION == "HORTON") {
    list_of_sections[['infiltration']] <- list("Horton", subcatchment) # infiltration
  } else {
    if (list_of_sections$options$INFILTRATION == "Green_Ampt" | list_of_sections$options$INFILTRATION == "GREEN_AMPT") {
      list_of_sections[['infiltration']] <- list("Green_Ampt", subcatchment)
    } else {
      warning("Function is only running with Horton or Green_Ampt infiltration.")
    }
  } 
  }

  # ... infiltration parameter
  if (is.null(infiltration)) {
    if (list_of_sections$options$INFILTRATION == "Horton" | list_of_sections$options$INFILTRATION == "HORTON") {
      if (!("MaxRate" %in% colnames(subcatchment)) | !("MinRate" %in% colnames(subcatchment)) | !("Decay" %in% colnames(subcatchment)) | !("DryTime" %in% colnames(subcatchment)) | !("MaxInfl" %in% colnames(subcatchment))) {
        warning("All or some Horton infiltration parameters are not defined, infiltration default values are taken.")
      }
    }
    if (list_of_sections$options$INFILTRATION == "Green_Ampt" | list_of_sections$options$INFILTRATION == "GREEN_AMPT") {
      if (!("Suction" %in% colnames(subcatchment)) | !("HydCon" %in% colnames(subcatchment)) | !("IMDmax" %in% colnames(subcatchment))) {
        warning("All or some Green_Ampt infiltration parameters are not defined, infiltration default values are taken.")
      }
    }
  } else {
    if (!("Soil" %in% colnames(subcatchment))) {
      stop("column Soil is missing in polygon.shp")
    }
  }

  # ... check for optional subcatchment_typologies:
  if (is.null(subcatchment_typologies)) {
    if (!("N_Imperv" %in% colnames(subcatchment)) | !("N_Perv" %in% colnames(subcatchment)) | !("S_Imperv" %in% colnames(subcatchment)) | !("S_Perv" %in% colnames(subcatchment)) | !("Pct_Zero" %in% colnames(subcatchment)) | !("RouteTo" %in% colnames(subcatchment)) | !("PctRouted" %in% colnames(subcatchment)) | !("Rain_Gage" %in% colnames(subcatchment)) | !("CurbLen" %in% colnames(subcatchment)) | !("Snowpack" %in% colnames(subcatchment)) | !("PercImperv" %in% colnames(subcatchment)) | !("Slope" %in% colnames(subcatchment)) | !("Width" %in% colnames(subcatchment))) {
      warning("N_Imperv, N_Perv, S_Imperv, S_Perv, Rain_Gage, CurbLen, Snowpack, PercImperv, Slope or Width are not defined in polygon.shp or Subcatchment_typologies. Check polygon.shp for completeness otherwise missing parameters in the sections subcatchment and subareas will be filled with default values.")
    }
  }
  if (!(is.null(subcatchment_typologies))) {
    if (!("Type" %in% colnames(subcatchment))) {
      stop("column Type is missing in polygon.shp")
    }
  }

  # ... and for the junction point shape:
  if (is.null(path_point) == T) {
    warning("Define path to point file including filename and ending otherwise sections junctions and coordinates are missing.")

    # specify object junction which is called when testing column names for warn message in junction_parameters
    junctions <- NULL
  } else {
    # read junction point file
    junctions <- sf::st_read(path_point, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()

    # check column names:
    if (all(c("Name", "Bottom") %in% colnames(junctions))) {
      if ("Top" %in% colnames(junctions) | "Ymax" %in% colnames(junctions)) {
        list_of_sections[["junctions"]] <- junctions
        list_of_sections[["coordinates"]] <- junctions[, c("Name", "geometry")]
      }
    } else {
      stop("The point file has to include at least the columns named: Name, Bottom and Top or Ymax.")
    }
  }


  if (is.null(junction_parameters)) {
    if (!("Y" %in% colnames(junctions)) | !("Ysur" %in% colnames(junctions)) | !("Apond" %in% colnames(junctions))) {
      warning(" Y, Ysur or Apond are not defined in point.shp or junction_parameters. Check point.shp for completeness otherwise missing parameters in the section junctions will be filled with default values.")
    }
  }

  # ... also do it for the outfall point shape:
  if (is.null(path_outfall)) {
    warning("Define path to outfall file including filename and ending otherwise section outfall is missing.")
  } else {
    # outfalls
    outfalls <- sf::st_read(path_outfall, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()
    # check for completeness:
    if (all(c("Name", "Bottom", "Type") %in% colnames(outfalls))) {
      list_of_sections[["outfalls"]] <- outfalls
      list_of_sections[["coordinates"]] <- rbind(list_of_sections[["coordinates"]], outfalls[, c("Name", "geometry")])
    } else {
      stop("The outfall point shape has to include at least the columns named: Name, Bottom, Type.")
    }
  }

  # ... checking, reading or adding default values for optional function arguments:

  # ... timeseries
  if (is.null(path_timeseries)) {
    warning("Define path to timeseries file including filename and ending, otherwise default values are taken.")
    list_of_sections[["timeseries"]] <- tibble::tibble(
      Name = "default_rain",
      date = " ",
      time_min = 1,
      rain_mm = 1
    )
  } else {
    # add path to timeseries file
    name_RG <- gsub("TIMESERIES ", "", list_of_sections[["raingages"]]$Source)
    list_of_sections[["timeseries"]] <- paste0(name_RG, " ", "FILE ", path_timeseries)
  }

  # ...add Pumps section if path_pump exists
  if (is.null(path_pumps) == F) {
    # read shape
    pumps <- sf::st_read(path_pumps, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()

    # add a section to list_of_sections
    list_of_sections[["pumps"]] <- pumps
  }

  # ... add pump curve
  if (is.null(path_pump_curve) == F) {
    # add table of pump curves
    list_of_sections[["curves"]] <- suppressMessages(readr::read_table2(path_pump_curve, col_names = c("Name", "Type", "X", "Y")))
  }

  # ...add weirs if path_weirs exists
  if (is.null(path_weirs) == F) {
    # read shape
    weirs <- sf::st_read(path_weirs, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()

    # add section to list_of_sections
    list_of_sections[["weirs"]] <- weirs
  }

  # ...add storages
  if (is.null(path_storage) == F) {
    # read shape
    storage <- sf::st_read(path_storage, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()
    
    # add section
    list_of_sections[["storage"]] <- storage
    list_of_sections[["coordinates"]] <- rbind(list_of_sections[["coordinates"]], storage[, c("Name", "geometry")])
  }
  
  # ... add storage curve
  if (is.null(path_storage_curve) == F) {
    if ("curves" %in% names(list_of_sections)) {
      # add table of storage curves to existing curve table
      storage_curves <- suppressMessages(readr::read_table2(path_storage_curve, col_names = c("Name", "Type", "X", "Y")))
      list_of_sections[["curves"]] <- rbind(list_of_sections[["curves"]], storage_curves)
    } else {
      # add table of storage curves to a new list entry
      list_of_sections[["curves"]] <- suppressMessages(readr::read_table2(path_storage_curve, col_names = c("Name", "Type", "X", "Y")))
    }
  }

  # ... do the same for the conduit line shape:
  if (is.null(path_line)) {
    warning("Define path to line file including filename and ending, otherwise section conduits is missing.")

    # specify object conduits which is called when testing column names for warn message in conduit_material
    conduits <- NULL
  } else {
    # read the line shape file
    # conduits
    conduits <- sf::st_read(path_line, stringsAsFactors = F, quiet = TRUE) %>%
      tibble::as_tibble() %>%
      compare_to_dictionary()
    # check column names
    if (all(c("Name", "Length", "Shape", "FromNode", "ToNode", "OutOffset", "Geom1") %in% colnames(conduits))) {
      list_of_sections[["conduits"]] <- conduits
      list_of_sections[["xsections"]] <- conduits
    } else {
      stop("The line file has to include at least the columns named: Name, Length, Shape, FromNode, ToNode, OutOffset, Geom1.")
    }
  }

  # ...check for material properties:
  if (is.null(conduit_material)) {
    if (!("Roughness" %in% colnames(conduits))) {
      warning("Roughness is not defined in line.shp or Conduit_material. Check line.shp for completeness otherwise missing parameters in the sections conduits will be filled with default values.")
    }
  } else {
    if (!("Material" %in% colnames(conduits))) {
      stop("column Material is missing in line.shp")
    }
  }

  # ...further processing of entries in list_of_sections...
  res <- list_of_sections %>%
    # define classes
    purrr::imap(function(.x, .y) {
      class(.x) <- c(.y, class(.x))
      return(.x)
    }) %>%
    # assign section parameters individually
    purrr::map(., ~ assign_parameters(.x, 
                                      infiltration, 
                                      subcatchment, 
                                      subcatchment_typologies, 
                                      conduit_material, 
                                      junction_parameters)) %>%
    # reclass to tibbles for consistency
    purrr::map( ~ {class(.x) <- c("tbl_df", "tbl", "data.frame");.x})

  # adjust order of sections
  section_order <-  c("title", "options", "evaporation", "raingages", "subcatchments", 
                      "subareas", "infiltration", "aquifers", "groundwater", 
                      "LID_controls", "LID_usage", "junctions", "outfalls", "storage", 
                      "conduits", "pumps", "weirs", "xsections", "controls", "DWF", 
                      "pollutants", "landuses", "coverages", "loadings", "buildup", 
                      "washoff", "inflows", "timeseries", "curves", "patterns",
                      "report", "tags", "map", "coordinates", "vertices", "polygons", 
                      "labels", "symbols", "backdrop")
  res <- res[section_order[section_order %in% names(res)]]
  
  # assign class attribute
  class(res) <- "inp"

  return(res)
}