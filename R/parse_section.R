#' import helper
#' @keywords internal
section_to_tbl <- function(x, section_name, rm.comment = FALSE, options = NULL) {
  
  # remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # remove comments
  if (rm.comment) x <- x[!startsWith(x, ";")]
  
  # convert character vector to tibble
  # todo:
  #Calling `as_tibble()` on a vector is discouraged, 
  #because the behavior is likely to change in the future. 
  #Use `enframe(name = NULL)` instead.
  x <- tibble::as_tibble(x) %>% 
    # remove empty lines
    dplyr::filter(value != "")
  
  # add section as class to prepare generic parser
  class(x) <- c(section_name, class(x))
  
  # generic parser
  if (section_name == "infiltration") {
    x <- parse_section(x, inf_model = tolower(options$INFILTRATION))
  } else {
    x <- parse_section(x)
  }

  # if a section is not parsed, we return NULL
  if (is.null(x)) return(NULL)
  
  # remove dummy columns which names starts with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # make sure ID columns are of type character
  chr_cols <- c("Name", "Link", "Links", "Subcatchment", "Outlet",
                "Node", "From Node", "To Node", "Gage", "Pump")
  
  for (chr_col in chr_cols) {
    if (chr_col %in% colnames(x)) {
      x <- dplyr::mutate_at(x, chr_col, as.character)
    }
  }
  
  # trimws of character columns
  x <- dplyr::mutate_if(x, is.character, trimws)
  
  # section class got lost while formatting to tibble, so add it again
  #class(x) <- c(section_name, class(x))
  
  # always return a tibble
  return(x)
}

# generic parser ----------------------------------------------------------

#' import helper
#' @keywords internal
parse_section <- function(x, ...) {
  UseMethod("parse_section", x)
} 

#' import helper
#' @keywords internal
parse_section.default <- function(x, ...) {
  warning(paste("Unknown class:", tail(class(x), 1)))
  #print(sloop::s3_dispatch(parse_section(x)))
  return(NULL)
}

#' helper function using defaults for arguments to tidyr::separate()
#' @keywords internal
separate_into <- function(
  x, into, fill = "left", sep = "\\s+", extra = "merge", col = "value", 
  convert = TRUE, ...
) {
  
  tidyr::separate(
    x, col = col, into = into, sep = sep, remove = TRUE, convert = convert, 
    extra = extra, fill = fill, ...
  )
}

#' helper function skippting the first n rows of a data frame
#' @keywords internal
skip_head <- function(df, n)
{
  df[-seq_len(n), ]
}

# input sections ----------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.options <- function(x, ...) {

  separate_into(x, c("Option", "Value"))
}

#' import helper
#' @keywords internal
parse_section.title <- function(x, ...) {
  
  x
}

#' import helper
#' @keywords internal
parse_section.raingages <- function(x, ...) {

  separate_into(x, c("Name", "Format", "Interval", "SCF", "Source"))
}

#' import helper
#' @keywords internal
parse_section.hydrographs <- function(x, ...) {

  separate_into(x, c(
    "Hydrograph", "Rain Gage/Month", "Response", "R", "T", "K", "Dmax", 
    "Drecov", "Dinit"
  ))
}

#' import helper
#' @keywords internal
parse_section.temperature <- function(x, ...) {
  
  separate_into(
    x, sep = base::cumsum(c(18, 1)), extra = "warn", fill = "warn",  
    into = c("Data Element", "tab1", "Values")
  )
}

#' import helper
#' @keywords internal
parse_section.evaporation <- function(x, ...) {
  
  separate_into(x, c("Data Source", "Parameters"))
}

#' import helper
#' @keywords internal
parse_section.events <- function(x, ...) {
  
  separate_into(x, sep = 19, extra = "warn", fill = "warn", into = c(
    "Start Date", "End Date"
  ))
}

#' import helper
#' @keywords internal
parse_section.subcatchments <- function(x, ...) {
  
  separate_into(x, sep = "\\s+", extra = "warn", fill = "warn", into = c(
    "Name", "Rain Gage", "Outlet", "Area","Perc_Imperv", "Width", 
    "Perc_Slope", "CurbLen", "Snowpack"
  ))
}

#' import helper
#' @keywords internal
parse_section.subareas <- function(x, ...) {
  
  separate_into(x, c("Subcatchment", "value")) %>% 
    separate_into(fill = "right", c(
      "N-Imperv", "N-Perv", "S-Imperv", "S-Perv", "PctZero", "RouteTo", 
      "PctRouted"
    ))
}

#' import helper
#' @keywords internal
parse_section.infiltration <- function(x, ...) {
  
  header <- switch(
    unlist(list(...)), 
    "horton" = c(
      "Subcatchment","MaxRate", "MinRate", "Decay", "DryTime", "MaxInfil"
    ), 
    "green_ampt" = c(
      "Subcatchment", "Suction", "Ksat", "IMD"
    ), 
    "curve_number" = c(
      "Subcatchment", "CurveNum", "empty", "DryTime"
    )
  )
  
  separate_into(x, header)
}

#' import helper
#' @keywords internal
parse_section.aquifers <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Name", "value")) %>% 
    separate_into(c(
      "Por", "WP", "FC", "Ksat", "Kslope", "Tslope", "ETu", "ETs", "Seep", 
      "Ebot", "Egw", "Umc", "ETupat"
    ))
}

#' import helper
#' @keywords internal
parse_section.snowpacks <- function(x, ...) {

  separate_into(x, c("Name", "Surface", "Parameters"))
}

#' import helper
#' @keywords internal
parse_section.junctions <- function(x, ...) {

  separate_into(x, sep = "\\s+", fill = "warn", into = c(
    "Name", "Elevation", "MaxDepth", "InitDepth", "SurDepth", "Aponded"
  ))
}

#' import helper
#' @keywords internal
parse_section.outfalls <- function(x, ...) {
  
  separate_into(x, c("Name", "value")) %>% 
    separate_into(
      sep = base::cumsum(c(10, 1, 10, 1, 10, 1, 10, 1)), extra = "warn", 
      fill = "warn", into = c(
        "Elevation", "tab2", "Type", "tab3", "Stage Data", "tab4", "Gated", 
        "tab5", "Route To"
      ))
}

#' import helper
#' @keywords internal
parse_section.dividers <- function(x, ...) {

  separate_into(x, c(
    "Name", "Elevation", "Diverted Link", "Type", "Parameters"
  ))
}

#' import helper
#' @keywords internal
parse_section.storage <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", c(
      "Elev.", "MaxDepth", "InitDepth", "Shape", "Curve Name/Params", "N/A", 
      "Fevap", "Psi", "Ksat", "IMD"
    ))
}

#' import helper
#' @keywords internal
parse_section.conduits <- function(x, ...) {
  
  separate_into(x, sep = "\\s+", fill = "warn", into = c(
    "Name", "From Node", "To Node", "Length", "Roughness", "InOffset", 
    "OutOffset", "InitFlow", "MaxFlow"
  ))
}

#' import helper
#' @keywords internal
parse_section.pumps <- function(x, ...) {
  
  separate_into(x, c(
    "Name", "From Node", "To Node", "Pump Curve", "Status", "Sartup", "Shutoff"
  ))
}

#' import helper
#' @keywords internal
parse_section.orifices <- function(x, ...) {

  separate_into(x, c(
    "Name", "From Node", "To Node", "Type", "Offset", "Qcoeff", "Gated", 
    "CloseTime"
  ))
}

#' import helper
#' @keywords internal
parse_section.weirs <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", c(
      "From Node", "To Node", "Type", "CrestHt", "Qcoeff", "Gated", "EndCon", 
      "EndCoeff", "Surcharge", "RoadWidth", "RoadSurf"
    ))
}

#' import helper
#' @keywords internal
parse_section.outlets <- function(x, ...) {
  
  separate_into(x, c(
    "Name", "From Node", "To Node", "Offset", "Type", "QTable/Qcoeff", "Qexpon", 
    "Gated"
  ))
}

#' import helper
#' @keywords internal
parse_section.xsections <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Link", "value")) %>% 
    separate_into(fill = "right", c(
      "Shape", "Geom1", "Geom2", "Geom3", "Geom4", "Barrels", "Culvert"
    ))
}

#' import helper
#' @keywords internal
parse_section.losses <- function(x, ...) {
  
  separate_into(x, c("Link", "Kentry", "Kexit", "Kavg", "Flap Gate", "Seepage"))
}

#' import helper
#' @keywords internal
parse_section.controls <- function(x, ...) {
  
  x
}

#' import helper
#' @keywords internal
parse_section.pollutants <- function(x, ...) {
  
  separate_into(x, fill = "right", c(
    "Name", "Units", "Crain", "Cgw", "Crdii", "Kdecay", "SnowOnly", 
    "Co-Pollutant", "Co-Frac", "Cdwf", "Cinit"
  ))
}

#' import helper
#' @keywords internal
parse_section.landuses <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Name", "value")) %>% 
    separate_into(fill = "right", c(
      "Sweeping_Interval", "Fraction_Available", "Last_Swept"
    ))
}

#' import helper
#' @keywords internal
parse_section.buildup <- function(x, ...) {
  
  separate_into(x, c(
    "LandUse", "Pollutant", "Function", "Coeff1", "Coeff2", "Coeff3", "Per_Unit"
  ))
}

#' import helper
#' @keywords internal
parse_section.washoff <- function(x, ...) {
  
  separate_into(x, c(
    "LandUse", "Pollutant", "Function", "Coeff1", "Coeff2", "SweepRmvl", 
    "BmpRmvl"
  ))
}

#' import helper
#' @keywords internal
parse_section.coverages <- function(x, ...) {
  
  separate_into(x, c("Subcatchment", "Land Use", "Percent"))
}

#' import helper
#' @keywords internal
parse_section.loadings <- function(x, ...) {
  
  separate_into(x, c("Subcatchment", "Pollutant", "Buildup"))
}

#' import helper
#' @keywords internal
parse_section.treatment <- function(x, ...) {
  
  separate_into(x, c("Node", "Pollutant", "Function"))
}

#' import helper
#' @keywords internal
parse_section.inflows <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Node", "value")) %>% 
    separate_into(fill = "right", c(
      "Constituent", "Time Series", "Type", "Mfactor", "Sfactor", "BaseLine", 
      "Pattern"
    ))
}

#' import helper
#' @keywords internal
parse_section.dwf <- function(x, ...) {
  
  separate_into(x, c("Node", "Constituent", "Baseline", "Patterns"))
}

#' import helper
#' @keywords internal
parse_section.iiflows <- function(x, ...) {
  
  separate_into(x, c("Node", "Unit Hydrograph", "Sewer Area"))
}

#' import helper
#' @keywords internal
parse_section.patterns <- function(x, ...) {
  
  separate_into(x, c("Name", "Type", "Multipliers"))
}

#' import helper
#' @keywords internal
parse_section.timeseries <- function(x, ...) {
  
  separate_into(x, c("Name", "value")) %>% 
    separate_into(c("Date", "Time", "Value"))
}

#' import helper
#' @keywords internal
parse_section.curves <- function(x, ...) {

  dplyr::mutate(x, value = trimws(value, which = "right")) %>% 
    separate_into(fill = "right", c("Name", "value")) %>% 
    separate_into(c("Type", "X-Value", "Y-Value"))
}

#' import helper
#' @keywords internal
parse_section.report <- function(x, ...) {
  
  separate_into(x, c("Reporting Options", "value"))
}

#' import helper
#' @keywords internal
parse_section.files <- function(x, ...) {
  
  separate_into(x, c("Verb", "Parameter", "Path"))
}

#' import helper
#' @keywords internal
parse_section.profiles <- function(x, ...) {
  
  separate_into(x, sep = "\" ", c("Name", "Links")) %>% 
    dplyr::mutate(Name = paste0(Name, "\""))
}

#' import helper
#' @keywords internal
parse_section.tags <- function(x, ...) {
  
  # warning: is there TAB missing for writing subcatchments in Uexport.pas?  
  # thus, we need a two step procedure
  separate_into(x, c("object", "id", "text"))
}

#' import helper
#' @keywords internal
parse_section.map <- function(x, ...) {
  
  separate_into(x, fill = "warn", c("key", "value"))
}

#' import helper
#' @keywords internal
parse_section.coordinates <- function(x, ...) {
  
  separate_into(x, c("Node", "X-Coord", "Y-Coord"))
}

#' import helper
#' @keywords internal
parse_section.vertices <- function(x, ...) {
  
  separate_into(x, c("Link", "X-Coord", "Y-Coord"))
}

#' import helper
#' @keywords internal
parse_section.polygons <- function(x, ...) {
  
  separate_into(x, c("Subcatchment", "X-Coord", "Y-Coord"))
}

#' import helper
#' @keywords internal
parse_section.symbols <- function(x, ...) {
  
  separate_into(x, c("Gage", "X-Coord", "Y-Coord"))
}

#' import helper
#' @keywords internal
parse_section.labels <- function(x, ...) {
  
  separate_into(x, c("X-Coord", "Y-Coord", "Label"))
}

#' import helper
#' @keywords internal
parse_section.lid_controls <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Name", "Type/Layer", "Parameters")) %>% 
    separate_into(fill = "right", col = "Parameters", into = paste0("Par", 1:7))
}

#' import helper
#' @keywords internal
parse_section.lid_usage <- function(x, ...) {
  
  separate_into(x, fill = "right", c(
    "Subcatchment", "LID Process", "Number", "Area", "Width", "InitSat", 
    "FromImp", "ToPerv", "RptFile", "DrainTo"
  ))
}

#' import helper
#' @keywords internal
parse_section.groundwater <- function(x, ...) {
  
  separate_into(x, fill = "right", c("Subcatchment", "value")) %>% 
    separate_into(fill = "right", c(
      "Aquifer", "Node", "Esurf", "A1", "B1", "A2", "B2", "A3", "Dsw", "Egwt",
      "Ebot", "Wgr", "Umc"
    ))
}

#' import helper
#' @keywords internal
parse_section.backdrop <- function(x, ...) {
  
  separate_into(x, c("Type", "Value"))
}

# report sections ---------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.element_count <- function(x, ...) {
  
  separate_into(skip_head(x, 3), fill = "right", sep = "\\.{3,}", c(
    "Element", "Count"
  ))
}

#' import helper
#' @keywords internal
parse_section.pollutant_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "Units", "Ppt_Concen", "GW_Concen", "Kdecay_per_day", "CoPollutant"
  ))
}

#' import helper
#' @keywords internal
parse_section.landuse_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "Sweeping_Interval", "Maximum_Removal", "Last_Swept"
  ))
}

#' import helper
#' @keywords internal
parse_section.raingage_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "Data_Source", "Data_Type", "Recording_Interval"
  ))
}

#' import helper
#' @keywords internal
parse_section.subcatchment_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "Area", "Width", "Perc_Imperv", "Perc_Slope", "Rain_Gage", "Outlet"
  ))
}

#' import helper
#' @keywords internal
parse_section.node_summary <- function(x, ...) {

  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "Type", "Invert_Elev", "Max_Depth", "Ponded_Area", "External_Inflow"
  ))
}

#' import helper
#' @keywords internal
parse_section.link_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Name", "From Node", "To Node", "Type", "Length", "Perc_Slope", "Roughness"
  ))
}

#' import helper
#' @keywords internal
parse_section.cross_section_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), fill = "right", c(
    "Conduit", "Shape", "Full_Depth", "Full_Area", "Hyd_Rad", "Max_Width", 
    "No_of_Barrels", "Full_Flow"
  ))
}

#' import helper
#' @keywords internal
parse_section.analysis_options <- function(x, ...) {
  
  separate_into(skip_head(x, 3), fill = "right", sep = "\\.{5,}", c(
    "Option", "Value"
  ))
}

#' import helper
#' @keywords internal
parse_section.runoff_quantity_continuity <- function(x, ...) {
  
  separate_into(skip_head(x, 2), sep = "\\.{5,}", c("Component", "value")) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume", "Depth"))
}

#' import helper
#' @keywords internal
parse_section.runoff_quality_continuity <- function(x, ...) {
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 2), sep = "\\.{5,}", c("Component", "value")) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(pollutants)
}

#' import helper
#' @keywords internal
parse_section.groundwater_continuity <- function(x, ...) {
  
  separate_into(skip_head(x, 2), sep = "\\.{4,}", c(
    "Component", "value"
  )) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume", "Depth"))
}

#' import helper
#' @keywords internal
parse_section.highest_continuity_errors <- function(x, ...) {
  
  separate_into(skip_head(x, 3), fill = "right", c(
    "Component", "Name", "Error"
  ))
}

#' import helper
#' @keywords internal
parse_section.time_step_critical_elements <- function(x, ...) {
  
  separate_into(skip_head(x, 3), fill = "right", c(
    "Component", "Name", "Value"
  ))
}

#' import helper
#' @keywords internal
parse_section.flow_routing_continuity <- function(x, ...) {

  separate_into(skip_head(x, 2), sep = "\\.{4,}", c("Component", "value")) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(c("Volume_a", "Volume_b"))
}

#' import helper
#' @keywords internal
parse_section.quality_routing_continuity <- function(x, ...) {
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 2), sep = "\\.{5,}", c("Component", "value")) %>% 
    dplyr::mutate_all(trimws) %>% 
    separate_into(pollutants)
}

#' import helper
#' @keywords internal
parse_section.highest_flow_instability_indexes <- function(x, ...) {
  
  separate_into(skip_head(x, 3), c("Link", "Instability"))
}

#' import helper
#' @keywords internal
parse_section.routing_time_step_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 3), sep = ":", c("Component", "Value"))
}

#' import helper
#' @keywords internal
parse_section.subcatchment_runoff_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 6), c("Subcatchment", paste(
    sep = "_", "Total", c(
      "Precip", "Runon", "Evap", "Infil", "Runoff_Depth", "Runoff_Volume", 
      "Peak_Runoff", "Runoff_Coeff"
    )
  )))
}

#' import helper
#' @keywords internal
parse_section.lid_performance_summary <- function(x, ...) {
  
  #c("Total","Evap","Infil","Surface","Drain","Initial","Final","Continuity")
  #c("Inflow","Loss","Loss","Outflow","Outflow","Storage","Storage","Error")

  separate_into(skip_head(x, 6), c(
    "Subcatchment","LID Control", 
    paste(
      c("Total", "Evap", "Infil", "Surface", "Drain", "Initial", "Final", 
        "Continuity"), 
      c("Inflow", "Loss", "Loss", "Outflow", "Outflow", "Storage", "Storage",
        "Error"), 
      sep = "_"
    )
  ))
}

#' import helper
#' @keywords internal
parse_section.subcatchment_washoff_summary <- function(x, ...) {
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 5), c("Subcatchment", pollutants))
}

#' import helper
#' @keywords internal
parse_section.node_depth_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 6), c(
    "Node", "Type", "Average_Depth", "Maximum_Depth", "Maximum_HGL", 
    "Time_of_Max_Occurance_d", "Time_of_Max_Occurance_hm", "Reported_Max_Depth"
  ))
}

#' import helper
#' @keywords internal
parse_section.node_inflow_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 7), c(
    "Node", "Type", "Maximum_Lateral_Inflow", "Maximum_Total_Inflow",
    "Time_of_Max_Occurance_d", "Time_of_Max_Occurance_hm", 
    "Lateral_Inflow_Volume", "Total_Inflow_Volume", "Flow_Balance_Error"
  ))
}

#' import helper
#' @keywords internal
parse_section.node_flooding_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 8), c(
    "Node", "Hours_Flooded", "Maximum_Rate", "Time_of_Max_Occurance_d", 
    "Time_of_Max_Occurance_hm", "Total_Flood_Volume", "Maximum_Ponded_Volume"
  ))
}

#' import helper
#' @keywords internal
parse_section.outfall_loading_summary <- function(x, ...) {
  
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
parse_section.link_flow_summary <- function(x, ...) {

  separate_into(skip_head(x, 6), fill = "right", c(
    "Link", "Type", "Maximum_Flow", "Time_of_Max_Occurance_d", 
    "Time_of_Max_Occurance_hm", "Maximum_Veloc", "Maximum_Full_Flow", 
    "Maximum_Full_Depth"
  ))
}

#' import helper
#' @keywords internal
parse_section.conduit_surcharge_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), c(
    "Conduit", "Hours_Full_Both_Ends", "Hours_Full_Upstream", 
    "Hours_Full_Dnstream", "Hours_Above_Full_Normal_Flow", 
    "Hours_Capacity_Limited"
  ))
}

#' import helper
#' @keywords internal
parse_section.link_pollutant_load_summary <- function(x, ...) {
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  separate_into(skip_head(x, 5), c("Link", pollutants))
}

#' import helper
#' @keywords internal
parse_section.pumping_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 6), c(
    "Pump", "Percent_Utilized", "Number_of_StartUps", "Min_Flow", "Avg_Flow", 
    "Max_Flow", "Total_Volume", "Power_Usage", "Time_Off_Pump_Curve_Low", 
    "Time_Off_Pump_Curve_High"
  ))
}

#' import helper
#' @keywords internal
parse_section.groundwater_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 7), c(
    "Subcatchment", "Total_Infil", "Total_Evap", "Total_Lower_Seepage", 
    "Total_Lateral_Outflow", "Maximum_Lateral_Outflow", "Average_Upper_Moist", 
    "Average_Water_Table", "Final_Upper_Moist", "Final_Water_Table"
  ))
}

#' import helper
#' @keywords internal
parse_section.node_surcharge_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 7), c(
    "Node", "Type", "Hours_Surcharged", "Max_Height_Above_Crown_Feet",
    "Min_Depth_Below_Rim_Feet"
  ))
}

#' import helper
#' @keywords internal
parse_section.flow_classification_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), c(
    "Conduit", "Adjusted_Actual_Length", "Dry", "Up_Dry", "Down_Dry", 
    "Sub_Crit", "Sup_Crit", "Up_Crit", "Down_Crit", "Norm_Ltd", "Inlet_Ctrl"
  ))
}

#' import helper
#' @keywords internal
parse_section.storage_volume_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 6), c(
    "Storage_Unit", "Average_Volume", "Avg_Pcnt_Full", "Evap_Pcnt_Loss", 
    "Exfil_Pcnt_Loss", "Maximum_Volume", "Max_Pcnt_Full", 
    "Time_of_Max_Occurence_days", "Time_of_Max_Occurence_hr_min",
    "Maximum_Outflow"
  ))
}

#' import helper
#' @keywords internal
parse_section.lid_control_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 5), c(
    "Subcatchment", "LID_Control", "No_of_Units", "Unit_Area", "Unit_Width", 
    "Percent_Area_Covered","Percent_Imperv_Treated", "Percent_Treated"
  ))
}

#' import helper
#' @keywords internal
parse_section.pumping_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 6), c(
    "Pump", "Percent_Utilized", "Number_of_Start_Ups", "Min_Flow", "Avg_Flow", 
    "Max_Flow", "Total_Volume", "Power_Usage", "Time_Off_Pump_Curve_Low", 
    "Time_Off_Pump_Curve_High"
  ))
}

#' import helper
#' @keywords internal
parse_section.groundwater_summary <- function(x, ...) {
  
  separate_into(skip_head(x, 7), c(
    "Subcatchment", "Total_Infil", "Total_Evap", "Total_Lower_Seepage", 
    "Total_Lateral_Outflow", "Maximum_Lateral_Outflow", "Average_Upper_Moist", 
    "Average_Water_Table", "Final_Upper_Moist", "Final_Water_Table"
  ))
}

#' import helper
#' @keywords internal
parse_section.rpt_error <- function(x, ...){
  
  # first line contains version string
  # currently not used (evtl. message?)
  version <- dplyr::slice(x, 1) %>% dplyr::pull(value)
  
  # remove version string
  x <- dplyr::slice(x, -1)
  
  # each error has two rows: error type and section
  odd <- dplyr::filter(x, dplyr::row_number() %% 2 == 1) %>% dplyr::pull(value)
  even <- dplyr::filter(x, dplyr::row_number() %% 2 == 0) %>% dplyr::pull(value)
  # we put all error in one row
  error <- tibble::tibble(value = paste(odd, even))
  
  # return errror
  return(error)
}