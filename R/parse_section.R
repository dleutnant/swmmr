#' import helper
#' @keywords internal
section_to_tbl <- function(x, section_name, rm.comment = FALSE, options = NULL)
{
  # Remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # Remove comments
  if (rm.comment) {
    x <- x[!startsWith(x, ";")]
  }
  
  # Convert character vector to tibble with class set to section_name
  x <- convert_to_section(x, section_name)

  # Call the generic parser
  x <- if (section_name == "infiltration") {
    parse_section(x, inf_model = tolower(options$INFILTRATION))
  } else {
    parse_section(x)
  }
  
  # Return NULL if the section was not parsed
  if (is.null(x)) {
    return(NULL)
  }

  # Remove dummy columns of which names start with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # Remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # Define names of ID columns (that are assumed to be of type character)
  chr_cols <- intersect(names(x), c(
    "Name", "Link", "Links", "Subcatchment", "Outlet", "Node", "From Node", 
    "To Node", "Gage", "Pump"
  ))

  # Make sure ID columns are of type character and trim whitespace characters
  x[chr_cols] <- lapply(x[chr_cols], function(x) trimws(as.character(x)))

  x
}

# convert_to_section -----------------------------------------------------------
convert_to_section <- function(x, section_name)
{
  # Remove empty lines, convert to one-column tibble and add section as class 
  # to prepare generic parser
  data.frame(value = x[x != ""]) %>%
    tibble::as_tibble() %>%
    add_class(section_name)
}

# generic parser ---------------------------------------------------------------

#' import helper
#' @keywords internal
parse_section <- function(x, ...)
{
  UseMethod("parse_section", x)
} 

#' import helper
#' @keywords internal
parse_section.default <- function(x, ...)
{
  warning(paste("Unknown class:", tail(class(x), 1)))
  #print(sloop::s3_dispatch(parse_section(x)))
  
  NULL
}

#' helper function using defaults for arguments to tidyr::separate()
#' @keywords internal
separate_into <- function(
    x, into, fill = "left", sep = "\\s+", extra = "merge", col = "value", 
    convert = TRUE, ...
)
{
  tidyr::separate(
    x, 
    col = col, 
    into = into, 
    sep = sep, 
    remove = TRUE, 
    convert = convert, 
    extra = extra, 
    fill = fill, 
    ...
  )
}

# input sections ---------------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.options <- function(x, ...)
{
  parse_standard(x, "options")
}

#' import helper
#' @keywords internal
parse_section.title <- function(x, ...)
{
  x
}

#' import helper
#' @keywords internal
parse_section.raingages <- function(x, ...)
{
  parse_standard(x, "raingages")
}

#' import helper
#' @keywords internal
parse_section.hydrographs <- function(x, ...)
{
  parse_standard(x, "hydrographs")
}

#' import helper
#' @keywords internal
parse_section.temperature <- function(x, ...)
{
  parse_standard(
    x, 
    "temperature", 
    sep = base::cumsum(c(18, 1)), 
    extra = "warn", 
    fill = "warn"
  )
}

#' import helper
#' @keywords internal
parse_section.evaporation <- function(x, ...)
{
  parse_standard(x, "evaporation")
}

#' import helper
#' @keywords internal
parse_section.events <- function(x, ...)
{
  parse_standard(x, "events", sep = 19, extra = "warn", fill = "warn")
}

#' import helper
#' @keywords internal
parse_section.subcatchments <- function(x, ...)
{
  parse_standard(
    x, 
    "subcatchments", 
    sep = "\\s+", 
    extra = "warn", 
    fill = "warn"
  )
}

#' import helper
#' @keywords internal
parse_section.subareas <- function(x, ...)
{
  x %>%
    separate_into(c("Subcatchment", "value")) %>% 
    separate_into(fill = "right", section_columns("subareas"))
}

#' import helper
#' @keywords internal
parse_section.infiltration <- function(x, ...)
{
  # Exactly one additional argument must be given in ...
  arguments <- list(...)
  stopifnot(length(arguments) == 1L)
  
  horton_header <- c("MaxRate", "MinRate", "Decay", "DryTime", "MaxInfil")
  green_ampt_header <- c("Suction", "Ksat", "IMD")
  curve_number_header <- c("CurveNum", "empty", "DryTime")
  
  header <- switch(
    arguments[[1L]], 
    horton = horton_header, 
    green_ampt = green_ampt_header, 
    modified_green_ampt = green_ampt_header,
    curve_number = curve_number_header
  )
  
  separate_into(x, c("Subcatchment", header))
}

#' import helper
#' @keywords internal
parse_section.aquifers <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(section_columns("aquifers"))
}

#' import helper
#' @keywords internal
parse_section.snowpacks <- function(x, ...)
{
  parse_standard(x, "snowpacks")
}

#' import helper
#' @keywords internal
parse_section.junctions <- function(x, ...)
{
  parse_standard(x, "junctions", sep = "\\s+", fill = "warn")
}

#' import helper
#' @keywords internal
parse_section.outfalls <- function(x, ...)
{
  x %>%
    separate_into(c("Name", "value")) %>% 
    separate_into(
      sep = base::cumsum(c(10, 1, 10, 1, 10, 1, 10, 1)), 
      extra = "warn", 
      fill = "warn", 
      into = section_columns("outfalls")
    )
}

#' import helper
#' @keywords internal
parse_section.dividers <- function(x, ...)
{
  parse_standard(x, "dividers")
}

#' import helper
#' @keywords internal
parse_section.storage <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", section_columns("storage"))
}

#' import helper
#' @keywords internal
parse_section.conduits <- function(x, ...)
{
  parse_standard(x, "conduits", sep = "\\s+", fill = "warn")
}

#' import helper
#' @keywords internal
parse_section.pumps <- function(x, ...)
{
  parse_standard(x, "pumps")
}

#' import helper
#' @keywords internal
parse_section.orifices <- function(x, ...)
{
  parse_standard(x, "orifices")
}

#' import helper
#' @keywords internal
parse_section.weirs <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", section_columns("weirs"))
}

#' import helper
#' @keywords internal
parse_section.outlets <- function(x, ...)
{
  parse_standard(x, "outlets")
}

#' import helper
#' @keywords internal
parse_section.xsections <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Link", "value")) %>% 
    separate_into(fill = "right", section_columns("xsections"))
}

#' import helper
#' @keywords internal
parse_section.losses <- function(x, ...)
{
  parse_standard(x, "losses")
}

#' import helper
#' @keywords internal
parse_section.controls <- function(x, ...)
{
  x
}

#' import helper
#' @keywords internal
parse_section.pollutants <- function(x, ...)
{
  parse_standard(x, "pollutants", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.landuses <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", section_columns("landuses"))
}

#' import helper
#' @keywords internal
parse_section.buildup <- function(x, ...)
{
  parse_standard(x, "buildup")
}

#' import helper
#' @keywords internal
parse_section.washoff <- function(x, ...)
{
  parse_standard(x, "washoff")
}

#' import helper
#' @keywords internal
parse_section.coverages <- function(x, ...)
{
  parse_standard(x, "coverages")
}

#' import helper
#' @keywords internal
parse_section.loadings <- function(x, ...)
{
  parse_standard(x, "loadings")
}

#' import helper
#' @keywords internal
parse_section.treatment <- function(x, ...)
{
  parse_standard(x, "treatment")
}

#' import helper
#' @keywords internal
parse_section.inflows <- function(x, ...)
{
  x %>%
    separate_into(fill = "right", c("Node", "value")) %>% 
    separate_into(fill = "right", section_columns("inflows"))
}

#' import helper
#' @keywords internal
parse_section.dwf <- function(x, ...)
{
  parse_standard(x, "dwf")
}

#' import helper
#' @keywords internal
parse_section.iiflows <- function(x, ...)
{
  parse_standard(x, "iiflows")
}

#' import helper
#' @keywords internal
parse_section.patterns <- function(x, ...)
{
  parse_standard(x, "patterns")
}

#' import helper
#' @keywords internal
parse_section.timeseries <- function(x, ...)
{
  x %>%
    separate_into(c("Name", "value")) %>% 
    separate_into(section_columns("timeseries"))
}

#' import helper
#' @keywords internal
parse_section.curves <- function(x, ...)
{
  x %>%
    dplyr::mutate(value = trimws(value, which = "right")) %>% 
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(section_columns("curves"))
}

#' import helper
#' @keywords internal
parse_section.report <- function(x, ...)
{
  separate_into(x, c("Reporting Options", "value"))
}

#' import helper
#' @keywords internal
parse_section.files <- function(x, ...)
{
  separate_into(x, c("Verb", "Parameter", "Path"))
}

#' import helper
#' @keywords internal
parse_section.profiles <- function(x, ...)
{
  separate_into(
    x = x, 
    sep = "\" ", 
    into = c("Name", "Links")
  ) %>% 
    dplyr::mutate(Name = paste0(Name, "\""))
}

#' import helper
#' @keywords internal
parse_section.tags <- function(x, ...)
{
  # warning: is there TAB missing for writing subcatchments in Uexport.pas?  
  # thus, we need a two step procedure
  separate_into(
    x = x, 
    into = c("object", "id", "text")
  )
}

#' import helper
#' @keywords internal
parse_section.map <- function(x, ...)
{
  separate_into(
    x = x, 
    fill = "warn", 
    into = c("key", "value")
  )
}

#' import helper
#' @keywords internal
parse_section.coordinates <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("Node", "X-Coord", "Y-Coord")
  )
}

#' import helper
#' @keywords internal
parse_section.vertices <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("Link", "X-Coord", "Y-Coord")
  )
}

#' import helper
#' @keywords internal
parse_section.polygons <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("Subcatchment", "X-Coord", "Y-Coord")
  )
}

#' import helper
#' @keywords internal
parse_section.symbols <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("Gage", "X-Coord", "Y-Coord")
  )
}

#' import helper
#' @keywords internal
parse_section.labels <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("X-Coord", "Y-Coord", "Label")
  )
}

#' import helper
#' @keywords internal
parse_section.lid_controls <- function(x, ...)
{
  x %>%
    separate_into(
      fill = "right", 
      into = c("Name", "Type/Layer", "Parameters")
    ) %>% 
    separate_into(
      fill = "right", 
      col = "Parameters", 
      into = paste0("Par", 1:7)
    )
}

#' import helper
#' @keywords internal
parse_section.lid_usage <- function(x, ...)
{
  parse_standard(x, "lid_usage", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.groundwater <- function(x, ...)
{
  x %>%  
    separate_into(
      fill = "right", 
      into = c("Subcatchment", "value")
    ) %>% 
    separate_into(
      fill = "right", 
      into = section_columns("groundwater")
    )
}

#' import helper
#' @keywords internal
parse_section.backdrop <- function(x, ...)
{
  separate_into(
    x = x, 
    into = c("Type", "Value")
  )
}

# report sections ---------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.element_count <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    fill = "right", 
    sep = "\\.{3,}", 
    into = c("Element", "Count")
  )
}

#' import helper
#' @keywords internal
parse_section.pollutant_summary <- function(x, ...)
{
  parse_standard(x, "pollutant_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.landuse_summary <- function(x, ...)
{
  parse_standard(x, "landuse_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.raingage_summary <- function(x, ...)
{
  parse_standard(x, "raingage_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.subcatchment_summary <- function(x, ...)
{
  parse_standard(x, "subcatchment_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.node_summary <- function(x, ...)
{
  parse_standard(x, "node_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.link_summary <- function(x, ...)
{
  parse_standard(x, "link_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.cross_section_summary <- function(x, ...)
{
  parse_standard(x, "cross_section_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.analysis_options <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    fill = "right", 
    sep = "\\.{5,}", 
    into = c("Option", "Value")
  )
}

#' import helper
#' @keywords internal
parse_section.runoff_quantity_continuity <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 2), 
    sep = "\\.{5,}", 
    into = c("Component", "value")
  ) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume", "Depth"))
}

#' import helper
#' @keywords internal
parse_section.runoff_quality_continuity <- function(x, ...)
{
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(
    x = skip_head(x, 2), 
    sep = "\\.{5,}", 
    into = c("Component", "value")
  ) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(pollutants)
}

#' import helper
#' @keywords internal
parse_section.groundwater_continuity <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 2), 
    sep = "\\.{4,}", 
    into = c("Component", "value")
  ) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume", "Depth"))
}

#' import helper
#' @keywords internal
parse_section.highest_continuity_errors <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    fill = "right", 
    into = c("Component", "Name", "Error")
  )
}

#' import helper
#' @keywords internal
parse_section.time_step_critical_elements <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    fill = "right", 
    into = c("Component", "Name", "Value")
  )
}

#' import helper
#' @keywords internal
parse_section.flow_routing_continuity <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 2), 
    sep = "\\.{4,}", 
    into = c("Component", "value")
  ) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume_a", "Volume_b"))
}

#' import helper
#' @keywords internal
parse_section.quality_routing_continuity <- function(x, ...)
{
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(
    x = skip_head(x, 2), 
    sep = "\\.{5,}", 
    into = c("Component", "value")
  ) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(pollutants)
}

#' import helper
#' @keywords internal
parse_section.highest_flow_instability_indexes <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    into = c("Link", "Instability")
  )
}

#' import helper
#' @keywords internal
parse_section.routing_time_step_summary <- function(x, ...)
{
  separate_into(
    x = skip_head(x, 3), 
    sep = ":", 
    into = c("Component", "Value")
  )
}

#' import helper
#' @keywords internal
parse_section.subcatchment_runoff_summary <- function(x, ...)
{
  parse_standard(x, "subcatchment_runoff_summary")
}

#' import helper
#' @keywords internal
parse_section.lid_performance_summary <- function(x, ...)
{
  parse_standard(x, "lid_performance_summary")
}

#' import helper
#' @keywords internal
parse_section.subcatchment_washoff_summary <- function(x, ...)
{
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 5), c("Subcatchment", pollutants))
}

#' import helper
#' @keywords internal
parse_section.node_depth_summary <- function(x, ...)
{
  parse_standard(x, "node_depth_summary")
}

#' import helper
#' @keywords internal
parse_section.node_inflow_summary <- function(x, ...)
{
  parse_standard(x, "node_inflow_summary")
}

#' import helper
#' @keywords internal
parse_section.node_flooding_summary <- function(x, ...)
{
  parse_standard(x, "node_flooding_summary")
}

#' import helper
#' @keywords internal
parse_section.outfall_loading_summary <- function(x, ...)
{
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[5, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.) %>% 
    .[-c(1:4)]
  
  #modify into vector if no pollutants 
  into <- c("Outfall_Node", "Flow_Freq", "Avg_Flow", "Max_Flow", "Total_Volume")
  
  if (length(pollutants) > 0) {
    into <- c(into, paste("Total", pollutants, sep = "_"))
  }
  
  separate_into(skip_head(x, 6), fill = "right", into)
}

#' import helper
#' @keywords internal
parse_section.link_flow_summary <- function(x, ...)
{
  parse_standard(x, "link_flow_summary", fill = "right")
}

#' import helper
#' @keywords internal
parse_section.conduit_surcharge_summary <- function(x, ...)
{
  parse_standard(x, "conduit_surcharge_summary")
}

#' import helper
#' @keywords internal
parse_section.link_pollutant_load_summary <- function(x, ...)
{
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 5), c("Link", pollutants))
}

#' import helper
#' @keywords internal
parse_section.pumping_summary <- function(x, ...)
{
  parse_standard(x, "pumping_summary")
}

#' import helper
#' @keywords internal
parse_section.groundwater_summary <- function(x, ...)
{
  parse_standard(x, "groundwater_summary")
}

#' import helper
#' @keywords internal
parse_section.node_surcharge_summary <- function(x, ...)
{
  parse_standard(x, "node_surcharge_summary")
}

#' import helper
#' @keywords internal
parse_section.flow_classification_summary <- function(x, ...)
{
  parse_standard(x, "flow_classification_summary")
}

#' import helper
#' @keywords internal
parse_section.storage_volume_summary <- function(x, ...)
{
  parse_standard(x, "storage_volume_summary")
}

#' import helper
#' @keywords internal
parse_section.lid_control_summary <- function(x, ...)
{
  parse_standard(x, "lid_control_summary")
}

#' import helper
#' @keywords internal
parse_section.rpt_error <- function(x, ...)
{
  # first line contains version string
  # currently not used (evtl. message?)
  version <- dplyr::slice(x, 1L) %>% 
    dplyr::pull(value)
  
  # remove version string
  x <- dplyr::slice(x, -1L)
  
  # each error has two rows: error type and section
  remainders <- dplyr::row_number(x) %% 2L
  
  odd <- dplyr::filter(x, remainders == 1L) %>% 
    dplyr::pull(value)
  
  even <- dplyr::filter(x, remainders == 0L) %>% 
    dplyr::pull(value)
  
  # return tibble with all error put in one row
  tibble::tibble(value = paste(odd, even))
}

# parse_standard ---------------------------------------------------------------
parse_standard <- function(x, key, ...)
{
  n_skip <- section_info(key)$n_skip
  
  if (n_skip > 0L) {
    x <- skip_head(x, n_skip)
  }
  
  separate_into(x, into = section_columns(key), ...)
}
