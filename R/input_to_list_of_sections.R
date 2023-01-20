#' Reads input data and checks data for completeness, returns a list_of_sections
#' which can be processed further.
#'
#' @keywords internal 
input_to_list_of_sections <- function(
    path_options, 
    subcatchment = NULL,
    subcatchment_typologies = NULL, 
    junctions = NULL,
    junction_parameters = NULL, 
    outfalls = NULL,
    conduits = NULL,
    conduit_material = NULL, 
    path_timeseries = NULL, 
    infiltration = NULL, 
    pumps = NULL, 
    path_pump_curve = NULL, 
    weirs = NULL,
    storage = NULL,
    path_storage_curve = NULL
)
{
  # check missing arguments, add default or generate error messages, in some
  # cases default values are added later...
  
  # if options are not available, add default values for sections "options",
  # "report", "evaporation"
  
  if (is.null(path_options)) {
    
    clean_warning("Options are missing, default values are taken.")
    
    list_of_sections <- list(
      options = default_options(),
      report = default_report(),
      evaporation = default_evaporation()
    )
    
  } else {
    
    list_of_sections <- read_list_of_sections(path_options)
  }
  
  if (!is.null(subcatchment)) {
    
    # check subcatchment for completeness and read supplementary information for
    # subcatchments (subcatchment_typologies and infiltration):
    
    # special case which should only occur if an inp has been exported using
    # swmmr. in this case Area_subcatchment is Ar_sbct and Area_LID_usage is
    # Ar_ld_s. Function was required to allow automatic tests with Example4.inp.
    #subcatchment <- data.frame(Ar_sbct = 1, b = 2, Area.subcatchment = 3)
    colnames(subcatchment) <- replace_values(
      x = colnames(subcatchment), 
      from = c("Ar_sbct", "Area.subcatchment"),
      to = c("Area", "Area")
    )

    # check the structure of polygon file:
    required_columns <- c("Name", "Outlet", "Area", "RouteTo")
    
    if (!all(required_columns %in% colnames(subcatchment))) {
      clean_stop(
        "The polygon shape has to include at least the columns named: ", 
        paste(required_columns, collapse = ", "), 
        ". For optional column names check the documentation."
      )
    }
    
    list_of_sections[['subcatchments']]  <- subcatchment
    list_of_sections[['subareas']] <- subcatchment
    list_of_sections[['polygons']] <- subcatchment
    
    # check infiltration model
    infiltration_model <- list_of_sections$options$INFILTRATION
    
    if (infiltration_model %in% c("Horton", "HORTON")) {
      
      list_of_sections[['infiltration']] <- list("Horton", subcatchment)
      
    } else if (infiltration_model %in% c("Green_Ampt", "GREEN_AMPT")) {
      
      list_of_sections[['infiltration']] <- list("Green_Ampt", subcatchment)
      
    } else {
      
      clean_warning(
        "Function is only running with Horton or Green_Ampt infiltration."
      )
    }
    
    # ... infiltration parameter
    if (is.null(infiltration)) {
      
      if (infiltration_model %in% c("Horton", "HORTON")) {
        
        if (!("MaxRate" %in% colnames(subcatchment)) | 
            !("MinRate" %in% colnames(subcatchment)) | 
            !("Decay" %in% colnames(subcatchment)) | 
            !("DryTime" %in% colnames(subcatchment)) | 
            !("MaxInfl" %in% colnames(subcatchment))) {
          
          clean_warning(
            "All or some Horton infiltration parameters are not defined, ", 
            "infiltration default values are taken."
          )
        }
      }
      
      if (infiltration_model %in% c("Green_Ampt", "GREEN_AMPT")) {
        
        if (!("Suction" %in% colnames(subcatchment)) | 
            !("HydCon" %in% colnames(subcatchment)) | 
            !("IMDmax" %in% colnames(subcatchment))) {
          
          clean_warning(
            "All or some Green_Ampt infiltration parameters are not defined, ",
            "infiltration default values are taken."
          )
        }
      }
      
    } else {
      
      if (!("Soil" %in% colnames(subcatchment))) {
        clean_stop("column Soil is missing in polygon shape")
      }
    }
    
    # ... check for optional subcatchment_typologies:
    if (is.null(subcatchment_typologies)) {

      required_columns <- c(
        "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", 
        "PctRouted", "Rain_Gage", "CurbLen", "Snowpack", "PercImperv", "Slope", 
        "Width"
      )
      
      if (!all(required_columns %in% colnames(subcatchment))) {
        
        clean_warning(
          paste(required_columns, collapse = ", "), 
          "are not all defined in polygon.shp or Subcatchment_typologies. ", 
          "Check polygon shape for completeness, otherwise missing parameters ", 
          "in the sections subcatchment and subareas will be filled with ", 
          "default values."
        )
      }
    }
    
    if (!(is.null(subcatchment_typologies))) {
      
      if (!("Type" %in% colnames(subcatchment))) {
        clean_stop("column Type is missing in polygon shape")
      }
    }
  }
  
  if (!is.null(junctions)) {
    
    # ... and for the junction point shape:
    # check column names:
    if (all(c("Name", "Bottom") %in% colnames(junctions))) {
      
      if ("Top" %in% colnames(junctions) | "Ymax" %in% colnames(junctions)) {
        list_of_sections[["junctions"]] <- junctions
        list_of_sections[["coordinates"]] <- junctions[, c("Name", "geometry")]
      }
      
    } else {
      
      clean_stop(
        "The point shape has to include at least the columns named: ", 
        "Name, Bottom and Top or Ymax."
      )
    }
    
    if (is.null(junction_parameters)) {
      
      if (!("Y" %in% colnames(junctions)) | 
          !("Ysur" %in% colnames(junctions)) | 
          !("Apond" %in% colnames(junctions))) {
        
        clean_warning(
          " Y, Ysur or Apond are not defined in point.shp (or point_sf) or ", 
          "junction_parameters. Check point shape for completeness otherwise ", 
          "missing parameters in the section junctions will be filled with ", 
          "default values."
        )
      }
    }
  }
  
  if (!is.null(outfalls)) {
    # ... also do it for the outfall point shape:
    # check for completeness:
    if (all(c("Name", "Bottom", "Type") %in% colnames(outfalls))) {
      
      list_of_sections[["outfalls"]] <- outfalls
      
      list_of_sections[["coordinates"]] <- rbind(
        list_of_sections[["coordinates"]], 
        outfalls[, c("Name", "geometry")]
      )
      
    } else {
      
      clean_stop(
        "The outfall point shape has to include at least the columns named: ", 
        "Name, Bottom, Type."
      )
    }
  }
  
  # checking, reading or adding default values for optional function arguments:
  
  # ... timeseries
  if (is.null(path_timeseries)) {
    
    clean_warning(
      "Define path to timeseries file including filename and ending, ", 
      "otherwise default values are taken."
    )
    
    list_of_sections[["timeseries"]] <- tibble::tibble(
      Name = "default_rain",
      date = " ",
      time_min = 1,
      rain_mm = 1
    )
    
  } else {
    
    # add path to timeseries file
    name_RG <- gsub("TIMESERIES ", "", list_of_sections[["raingages"]]$Source)
    
    list_of_sections[["timeseries"]] <- paste0(
      name_RG, " ", "FILE ", path_timeseries
    )
  }
  
  # ...add Pumps section if path_pump or pumps_sf exists
  if (!is.null(pumps)) {
    # add a section to list_of_sections
    list_of_sections[["pumps"]] <- pumps
  }
  
  # ... add pump curve
  if (!is.null(path_pump_curve)) {
    # add table of pump curves
    list_of_sections[["curves"]] <- suppressMessages(readr::read_table2(
      file = path_pump_curve, 
      col_names = c("Name", "Type", "X", "Y")
    ))
  }
  
  # ...add weirs if path_weirs or weirs_sf exists
  if (!is.null(weirs)) {
    # add section to list_of_sections
    list_of_sections[["weirs"]] <- weirs
  }
  
  # ...add storages if path_storage or storage_sf exists
  if (!is.null(storage)){
    
    # add section
    list_of_sections[["storage"]] <- storage
    
    list_of_sections[["coordinates"]] <- rbind(
      list_of_sections[["coordinates"]], 
      storage[, c("Name", "geometry")]
    )
  }
  
  # ... add storage curve
  if (!is.null(path_storage_curve)) {
    
    if ("curves" %in% names(list_of_sections)) {
      
      # add table of storage curves to existing curve table
      storage_curves <- suppressMessages(readr::read_table2(
        file = path_storage_curve, 
        col_names = c("Name", "Type", "X", "Y")
      ))
      
      list_of_sections[["curves"]] <- rbind(
        list_of_sections[["curves"]], 
        storage_curves
      )
      
    } else {
      
      # add table of storage curves to a new list entry
      list_of_sections[["curves"]] <- suppressMessages(readr::read_table2(
        file = path_storage_curve, 
        col_names = c("Name", "Type", "X", "Y")
      ))
    }
  }
  
  if (!is.null(conduits)) {
    
    # ... do the same for the conduit line shape: check column names
    required_columns <- c(
      "Name", "Length", "Shape", "FromNode", "ToNode", "OutOffset", "Geom1"
    )
    
    if (all(required_columns %in% colnames(conduits))) {
      
      list_of_sections[["conduits"]] <- conduits
      list_of_sections[["xsections"]] <- conduits
      
    } else {
      
      clean_stop(
        "The line shape has to include at least the columns named: ",
        paste(required_columns, collapse = ", "),
        "."
      )
    }
    
    # ...check for material properties:
    if (is.null(conduit_material)) {
      
      if (!("Roughness" %in% colnames(conduits))) {
        clean_warning(
          "Roughness is not defined in line shape or Conduit_material. ", 
          "Check line shape for completeness otherwise missing parameters in ", 
          "the sections conduits will be filled with default values."
        )
      }
      
    } else {
      
      if (!("Material" %in% colnames(conduits))) {
        clean_stop("column Material is missing in line shape")
      }
    }
  }
  
  list_of_sections
}

# default_options --------------------------------------------------------------
default_options <- function()
{
  tibble::tibble(
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
}

# default_report ---------------------------------------------------------------
default_report <- function()
{
  tibble::tibble(
    INPUT = "NO",
    CONTROLS = "NO",
    SUBCATCHMENTS = "ALL",
    NODES = "ALL",
    LINKS = "ALL"
  )
}

# default_evaporation ----------------------------------------------------------
default_evaporation <- function()
{
  tibble::tibble(
    CONSTANT = 0,
    DRY_ONLY = 0
  )
}

# read_list_of_sections --------------------------------------------------------
read_list_of_sections <- function(path_options)
{
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
      tibble::as_tibble(.) %>%
      tidyr::separate(., 1, c("Variable", "Value"), "\t", extra = "merge", fill = "left")
    
    # define column vector to keep the order later
    cols <- list_of_sections[[i]][, 1] %>% unlist(.) %>% as.character(.)
    
    # transpose and keep order of original data
    list_of_sections[[i]] <- tidyr::gather(list_of_sections[[i]], Variable, Value) %>%
      tidyr::spread(names(.)[1], "Value") %>%
      .[, cols]
    
    # separate rows with more than one entry (e.g. in pollution section)
    if (any(grepl("\t", list_of_sections[[i]]))) {
      list_of_sections[[i]] <- tidyr::separate_rows(list_of_sections[[i]], (1:ncol(list_of_sections[[i]])), sep = "\t")
    }
  }
  
  list_of_sections
}
