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
  # Define helper functions
  silently_read <- function(file, col_names) {
    suppressMessages(readr::read_table2(file = file, col_names = col_names))
  }
  
  # Check for missing arguments, add default or generate error messages. 
  # In some cases default values are added later.
  
  # Define sections for which to use defaults if options are not available
  default_sections <- c("options", "report", "evaporation")
  
  # If options are not available, add default values for sections 
  list_of_sections <- if (is.null(path_options)) {
    
    clean_warning("Options are missing, default values are taken.")
    lapply(get_column_defaults()[default_sections], tibble::as_tibble)
    
  } else {
    
    read_list_of_sections(path_options)
  }
  
  if (!is.null(subcatchment)) {
    
    # Check subcatchment for completeness and read supplementary information for
    # subcatchments (subcatchment_typologies and infiltration)
    
    # Special case which should only occur if an inp has been exported using
    # swmmr. In this case Area_subcatchment is Ar_sbct and Area_LID_usage is
    # Ar_ld_s. Function was required to allow automatic tests with Example4.inp.
    colnames(subcatchment) <- replace_values(
      x = colnames(subcatchment), 
      from = c("Ar_sbct", "Area.subcatchment"),
      to = c("Area", "Area")
    )
    
    # Check the structure of polygon file
    stop_if_shape_does_not_include(
      required = c("Name", "Outlet", "Area", "RouteTo"),
      data = subcatchment,
      shape = "polygon",
      "For optional column names check the documentation."
    )
    
    list_of_sections[['subcatchments']]  <- subcatchment
    list_of_sections[['subareas']] <- subcatchment
    list_of_sections[['polygons']] <- subcatchment
    
    # Check infiltration model
    infiltration_model <- list_of_sections$options$INFILTRATION
    
    is_horton <- infiltration_model %in% c("Horton", "HORTON")
    is_green_ampt <- infiltration_model %in% c("Green_Ampt", "GREEN_AMPT")
    
    if (is_horton || is_green_ampt) {
      
      list_of_sections[['infiltration']] <- list(
        ifelse(is_horton, "Horton", "Green_Ampt"), 
        subcatchment
      )
      
    } else {
      
      clean_warning(
        "Function is only running with Horton or Green_Ampt infiltration."
      )
    }
    
    # Check for infiltration parameters
    if (is.null(infiltration)) {
      
      if (is_horton || is_green_ampt) {
        
        warn_if_not_defined(
          required = if (is_horton) {
            c("MaxRate", "MinRate", "Decay", "DryTime", "MaxInfl")
          } else {
            c("Suction", "HydCon", "IMDmax")
          }, 
          subcatchment, 
          "All or some ", ifelse(is_horton, "Horton", "Green_Ampt"), 
          " infiltration parameters ", 
          "are not defined, infiltration default values are taken."
        )
      }
      
    } else {
      
      stop_if_column_not_in_shape("Soil", subcatchment, "polygon")
    }
    
    # Check for optional subcatchment_typologies
    if (is.null(subcatchment_typologies)) {
      warn_if_not_defined_check(
        required = c(
          "N_Imperv", "N_Perv", "S_Imperv", "S_Perv", "Pct_Zero", "RouteTo", 
          "PctRouted", "Rain_Gage", "CurbLen", "Snowpack", "PercImperv", 
          "Slope", "Width"
        ),
        data = subcatchment,
        where = "polygon.shp or Subcatchment_typologies",
        shape = "polygon", 
        sections = "subcatchment and subareas"
      )
    }
    
    if (!(is.null(subcatchment_typologies))) {
      stop_if_column_not_in_shape(
        column = "Type", 
        data = subcatchment, 
        shape = "polygon"
      )
    }
  }
  
  if (!is.null(junctions)) {
    
    # Check column names for the junction point shape
    stop_if_shape_does_not_include(
      required = c("Name", "Bottom"), 
      data = junctions, 
      shape = "point", 
      extra = " and Top or Ymax"
    )
    
    if (any(c("Top", "Ymax") %in% colnames(junctions))) {
      list_of_sections[["junctions"]] <- junctions
      list_of_sections[["coordinates"]] <- junctions[, c("Name", "geometry")]
    }
    
    if (is.null(junction_parameters)) {
      
      warn_if_not_defined_check(
        required = c("Y", "Ysur", "Apond"),
        data = junctions,
        where = "point.shp (or point_sf) or junction_parameters", 
        shape = "point", 
        sections = "junctions"
      )
    }
  }
  
  if (!is.null(outfalls)) {
    
    # Check the outfall point shape for completeness
    stop_if_shape_does_not_include(
      required = c("Name", "Bottom", "Type"), 
      data =  outfalls, 
      shape = "outfall point"
    )
    
    list_of_sections[["outfalls"]] <- outfalls
    
    list_of_sections[["coordinates"]] <- rbind(
      list_of_sections[["coordinates"]], 
      outfalls[, c("Name", "geometry")]
    )
  }
  
  # Checking, reading or adding default values for optional function arguments
  
  # Timeseries
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
    
    # Add path to timeseries file
    name_RG <- gsub("TIMESERIES ", "", list_of_sections[["raingages"]]$Source)
    
    list_of_sections[["timeseries"]] <- paste0(
      name_RG, " ", "FILE ", path_timeseries
    )
  }
  
  # Add pumps section to list_of_sections if path_pump or pumps_sf exists
  if (!is.null(pumps)) {
    list_of_sections[["pumps"]] <- pumps
  }
  
  # Add table of pump curves
  if (!is.null(path_pump_curve)) {
    list_of_sections[["curves"]] <- silently_read(
      file = path_pump_curve, 
      col_names = c("Name", "Type", "X", "Y")
    )
  }
  
  # Add weirs section to list_of_sections if path_weirs or weirs_sf exists
  if (!is.null(weirs)) {
    list_of_sections[["weirs"]] <- weirs
  }
  
  # Add storages and coordinates section to list_of_sections if path_storage
  # or storage_sf exists
  if (!is.null(storage)) {
    
    list_of_sections[["storage"]] <- storage
    
    list_of_sections[["coordinates"]] <- rbind(
      list_of_sections[["coordinates"]], 
      storage[, c("Name", "geometry")]
    )
  }
  
  # Add storage curve
  if (!is.null(path_storage_curve)) {
    
    if ("curves" %in% names(list_of_sections)) {
      
      # Add table of storage curves to existing curve table
      storage_curves <- silently_read(
        file = path_storage_curve, 
        col_names = c("Name", "Type", "X", "Y")
      )
      
      list_of_sections[["curves"]] <- rbind(
        list_of_sections[["curves"]], 
        storage_curves
      )
      
    } else {
      
      # Add table of storage curves to a new list entry
      list_of_sections[["curves"]] <- silently_read(
        file = path_storage_curve, 
        col_names = c("Name", "Type", "X", "Y")
      )
    }
  }
  
  if (!is.null(conduits)) {
    
    # Check column names for the conduit line shape
    stop_if_shape_does_not_include(
      required = c(
        "Name", "Length", "Shape", "FromNode", "ToNode", "OutOffset", "Geom1"
      ), 
      data =  conduits,
      shape = "line"
    )
    
    list_of_sections[["conduits"]] <- conduits
    list_of_sections[["xsections"]] <- conduits
    
    # Check for material properties
    if (is.null(conduit_material)) {
      
      warn_if_not_defined_check(
        required = "Roughness",
        data = conduits,
        where = "line shape or Conduit_material", 
        shape = "line",
        sections = "conduits"
      )
      
    } else {
      
      stop_if_column_not_in_shape("Material", conduits, "line")
    }
  }
  
  list_of_sections
}

# read_list_of_sections --------------------------------------------------------
read_list_of_sections <- function(path_options)
{
  # Read options file
  options <- readLines(path_options)
  
  # Remove empty lines
  options <- options[options != ""]
  
  # Split text lines at [section headers]
  text_blocks <- extract_sections(options)
  
  # Process each section: make a tibble and transpose it
  lapply(text_blocks, function(text_block) {
    
    #text_block <- text_blocks[[1L]]
    
    # Create tibble    
    result <- text_block %>%
      tibble::as_tibble() %>%
      separate_into(c("Variable", "Value"), sep = "\t", col = 1L)
    
    # Define column vector to keep the order later
    cols <- dplyr::pull(result, 1L)
    
    # Transpose and keep order of original data
    result <- result %>%
      tidyr::gather(Variable, Value) %>%
      tidyr::spread(names(.)[1], "Value") %>%
      .[, cols]
    
    # Separate rows with multiple entries (e.g. in pollution section)
    if (any(grepl("\t", result))) {
      result <- result %>%
        tidyr::separate_rows(seq_len(ncol(result)), sep = "\t")
    }
    
    result
  })
}

# stop_if_shape_does_not_include -----------------------------------------------
stop_if_shape_does_not_include <- function(
    required, data, shape, ..., extra = NULL
)
{
  is_available <- required %in% colnames(data)
  
  if (!all(is_available)) {
    
    main_message <- sprintf(
      paste0(
        "The %s shape has to include at least these columns:\n  %s%s.\n",
        "Missing column(s):\n  %s."
      ),
      shape,
      comma_space_collapsed(required), 
      if (is.null(extra)) "" else extra,
      comma_space_collapsed(required[!is_available])
    )
    
    clean_stop(main_message, ...)
  }
}

# warn_if_not_defined ----------------------------------------------------------
warn_if_not_defined <- function(required, data, ...)
{
  if (!all(required %in% colnames(data))) {
    clean_warning(...)
  }
}

# stop_if_column_not_in_shape --------------------------------------------------
stop_if_column_not_in_shape <- function(column, data, shape)
{
  if (!(column %in% colnames(data))) {
    stop_formatted("column %s is missing in %s shape", column, shape)
  }
}

# warn_if_not_defined_check ----------------------------------------------------
warn_if_not_defined_check <- function(required, data, where, shape, sections)
{
  text_not_all_defined <- sprintf(
    if (length(required) == 1L) {
      "%s is not defined in %s. "
    } else {
      "Not all of %s are defined in %s. "
    },
    comma_space_collapsed(required), 
    where
  )
  
  text_check_shape <- sprintf(
    paste0(
      "Check %s shape for completeness, otherwise missing parameters in the ",
      "sections %s will be filled with default values."
    ),
    shape, 
    sections
  )
  
  warn_if_not_defined(
    required, 
    data, 
    text_not_all_defined, 
    text_check_shape
  )
}
