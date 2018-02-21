#' import helper
#' @keywords internal
section_to_tbl <- function(x, section_name, rm.comment = FALSE) {
  
  # special case for infiltration section
  if (section_name == "infiltration") {
    header <- x[startsWith(x, ";;")]
    if (any(grepl("MaxRate", header))) inf_model <- "horton"
    if (any(grepl("Suction", header))) inf_model <- "green_ampt"
    if (any(grepl("CurveNum", header))) inf_model <- "curve_number"
  }
  
  # remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # remove comments
  if (rm.comment) x <- x[!startsWith(x, ";")]
  
  # convert character vector to tibble
  x <- tibble::as_tibble(x) %>% 
    # remove empty lines
    dplyr::filter(value != "")
  
  # add section as class to prepare generic parser
  class(x) <- c(section_name, class(x))
  
  # generic parser
  x <- parse_section(x, inf_model = inf_model)
  
  # if a section is not parsed, we return NULL
  if (is.null(x)) return(NULL)
  
  # remove dummy columns which names starts with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # make sure ID columns are of type character
  chr_cols <- c("Name", "Link", "Links", "Subcatchment", "Outlet",
                "Node", "From Node", "To Node", "Gage")
  
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
  return(NULL)
}


# input sections ----------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.options <- function(x, ...){

  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Option", "Value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.title <- function(x, ...) {
  
  x
  
}

#' import helper
#' @keywords internal
parse_section.raingages <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Format", "Interval",
                           "SCF", "Source"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.hydrographs <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Hydrograph", "Rain Gage/Month", "Response",
                           "R", "T", "K",
                           "Dmax", "Drecov", "Dinit"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.temperature <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Data Element", "tab1", "Values"),
                  sep = base::cumsum(c(18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.evaporation <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Data Source", "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.events <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Start Date", "End Date"),
                  sep = 19, 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.subcatchments <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name",
                           "Rain Gage", 
                           "Outlet", "Area",
                           "Perc_Imperv", "Width",
                           "Perc_Slope", "CurbLen", 
                           "Snowpack"),
                  sep = "\\s+", 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.subareas <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Subcatchment", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("N-Imperv", 
                           "N-Perv", "S-Imperv",
                           "S-Perv", "PctZero", "RouteTo", 
                           "PctRouted"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.infiltration <- function(x, ...){
  
  header <- switch(unlist(list(...)), 
                   "horton" = c("Subcatchment","MaxRate", 
                                "MinRate", "Decay",
                                "DryTime", "MaxInfil"), 
                   "green_ampt" = c("Subcatchment", "Suction", 
                                    "Ksat", "IMD"), 
                   "curve_number" = c("Subcatchment", "CurveNum", 
                                      "empty", "DryTime"))
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = header,
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.aquifers <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Por", 
                             "WP", "FC",
                             "Ksat", "Kslope",
                             "Tslope", "ETu", 
                             "ETs", "Seep", 
                             "Ebot", "Egw",
                             "Umc", "ETupat"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.snowpacks <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Surface", "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.junctions <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Elevation", 
                           "MaxDepth", "InitDepth", 
                           "SurDepth", "Aponded"),
                  sep = "\\s+",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outfalls <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Elevation", "tab2",
                             "Type", "tab3", "Stage Data", "tab4",
                             "Gated", "tab5", "Route To"),
                    sep = base::cumsum(c(10, 1, 10, 1, 10, 1, 10, 1)), 
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.dividers <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Elevation", 
                           "Diverted Link", "Type", 
                           "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) 
  
}

#' import helper
#' @keywords internal
parse_section.storage <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value",
                    into = c("Elev.", 
                             "MaxDepth", "InitDepth", 
                             "Shape", "Curve Name/Params", 
                             "N/A", "Fevap", 
                             "Psi", "Ksat", "IMD"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.conduits <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Length", 
                           "Roughness", "InOffset", 
                           "OutOffset", "InitFlow", 
                           "MaxFlow"),
                  sep = "\\s+",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.pumps <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Pump Curve", 
                           "Status", "Sartup", 
                           "Shutoff"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.orifices <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Type", 
                           "Offset", "Qcoeff", 
                           "Gated", "CloseTime"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left", 
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.weirs <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", 
                           "To Node", "Type", 
                           "CrestHt", "Qcoeff", 
                           "Gated", "EndCon", 
                           "EndCoeff", "Surcharge",
                           "RoadWidth", "RoadSurf"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outlets <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Offset", 
                           "Type", "QTable/Qcoeff", 
                           "Qexpon", "Gated"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.xsections <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Link", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value",
                    into = c("Shape", 
                             "Geom1", "Geom2", 
                             "Geom3", "Geom4", 
                             "Barrels", "Culvert"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.losses <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "Kentry", 
                           "Kexit", "Kavg", 
                           "Flap Gate", "Seepage"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.controls <- function(x, ...){
  
  x
  
}

#' import helper
#' @keywords internal
parse_section.pollutants <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Units", 
                           "Crain", "Cgw", 
                           "Crdii", "Kdecay", 
                           "SnowOnly", "Co-Pollutant", 
                           "Co-Frac", "Cdwf", "Cinit"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.landuses <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Sweeping_Interval", 
                             "Fraction_Available", "Last_Swept"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.buildup <- function(x, ...){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("LandUse", "Pollutant", 
                           "Function", "Coeff1", 
                           "Coeff2", "Coeff3", 
                           "Per_Unit"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.washoff <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("LandUse", "Pollutant", 
                           "Function", "Coeff1", 
                           "Coeff2", "SweepRmvl", 
                           "BmpRmvl"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.coverages <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "Land Use", 
                           "Percent"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.loadings <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "Pollutant", 
                           "Buildup"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.treatment <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "Pollutant", 
                           "Function"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.inflows <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Node", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Constituent", "Time Series", "Type",
                             "Mfactor", "Sfactor", "BaseLine", "Pattern"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.dwf <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "Constituent", "Baseline", "Patterns"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.iiflows <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "Unit Hydrograph", "Sewer Area"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.patterns <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Type", "Multipliers"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.timeseries <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Date", "Time", "Value"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.curves <- function(x, ...){
  
  x %>% 
    dplyr::mutate(value = trimws(value, which = "right")) %>% 
    tidyr::separate(col = "value", 
                    into = c("Name", "value"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("Type", "X-Value", "Y-Value"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.report <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Reporting Options", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.files <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Verb", "Parameter", "Path"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.profiles <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Links"),
                  sep = "\" ",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    dplyr::mutate(Name = paste0(Name, "\""))
  
}

#' import helper
#' @keywords internal
parse_section.tags <- function(x, ...) {
  
  # warning: is there TAB missing for writing subcatchments in Uexport.pas?  
  # thus, we need a two step procedure
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("object", "id", "text"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.map <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("key", "value"),
                  extra = "merge",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.coordinates <- function(x, ...){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "X-Coord", "Y-Coord"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.vertices <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Link", "X-Coord", "Y-Coord"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.polygons <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "X-Coord", "Y-Coord"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.symbols <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Gage", "X-Coord", "Y-Coord"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.labels <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("X-Coord", "Y-Coord", "Label"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.lid_controls <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Type/Layer", "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
    tidyr::separate(col = "Parameters", 
                    into = paste0("Par", 1:7),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "right",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.lid_usage <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "LID Process", 
                           "Number", "Area", 
                           "Width", "InitSat", 
                           "FromImp", "ToPerv", 
                           "RptFile", "DrainTo"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.groundwater <- function(x, ...) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Subcatchment", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Aquifer", "Node", 
                           "Esurf", "A1", "B1", "A2", 
                           "B2", "A3", "Dsw", "Egwt",
                           "Ebot", "Wgr", "Umc"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.backdrop <- function(x, ...) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Type", "Value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

# report sections ---------------------------------------------------------

#' import helper
#' @keywords internal
parse_section.element_count <- function(x, ...){
  
  tidyr::separate(data = x[-(1:3), ],
                  col = "value",
                  into = c("Element", "Count"),
                  sep = "\\.{3,}",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.pollutant_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "Units", "Ppt_Concen", 
                           "GW_Concen", "Kdecay_per_day", 
                           "CoPollutant"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.landuse_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "Sweeping_Interval", 
                           "Maximum_Removal", "Last_Swept"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.raingage_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "Data_Source", 
                           "Data_Type", "Recording_Interval"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.subcatchment_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "Area", "Width", 
                           "Perc_Imperv", "Perc_Slope", 
                           "Rain_Gage", "Outlet"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.node_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "Type", "Invert_Elev", 
                           "Max_Depth", "Ponded_Area", 
                           "External_Inflow"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.link_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Name", "From Node", "To Node", 
                           "Type", "Length", 
                           "Perc_Slope", "Roughness"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.cross_section_summary <- function(x, ...){
  
  tidyr::separate(data = x[-(1:5), ],
                  col = "value",
                  into = c("Conduit", "Shape", "Full_Depth", 
                           "Full_Area", "Hyd_Rad", 
                           "Max_Width", "No_of_Barrels", 
                           "Full_Flow"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.analysis_options <- function(x, ...){
  
  tidyr::separate(data = x[-(1:3), ],
                  col = "value",
                  into = c("Option", "Value"),
                  sep = "\\.{5,}",
                  extra = "merge",
                  fill = "right",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.runoff_quantity_continuity <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:2), ],
                  col = "value",
                  into = c("Component", "value"),
                  sep = "\\.{5,}",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    dplyr::mutate_all(trimws) %>% 
    tidyr::separate(col = "value",
                    into = c("Volume", "Depth"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.runoff_quality_continuity <- function(x, ...){
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  tidyr::separate(data = x[-c(1:2), ],
                  col = "value",
                  into = c("Component", "value"),
                  sep = "\\.{5,}",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    dplyr::mutate_all(trimws) %>% 
    tidyr::separate(col = "value",
                    into = pollutants,
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.flow_routing_continuity <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:2), ],
                  col = "value",
                  into = c("Component", "value"),
                  sep = "\\.{4,}",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    dplyr::mutate_all(trimws) %>% 
    tidyr::separate(col = "value",
                    into = c("Volume_a", "Volume_b"),
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.quality_routing_continuity <- function(x, ...){
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[1, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  tidyr::separate(data = x[-c(1:2), ],
                  col = "value",
                  into = c("Component", "value"),
                  sep = "\\.{5,}",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    dplyr::mutate_all(trimws) %>% 
    tidyr::separate(col = "value",
                    into = pollutants,
                    sep = "\\s+",
                    extra = "merge",
                    fill = "left",
                    convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.highest_flow_instability_indexes <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:3), ],
                  col = "value",
                  into = c("Link", "Instability"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.routing_time_step_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:3), ],
                  col = "value",
                  into = c("Component", "Value"),
                  sep = ":",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.subcatchment_runoff_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:6),],
                  col = "value",
                  into = c("Subcatchment", 
                           paste("Total", c("Precip", "Runon", "Evap", "Infil",
                                            "Runoff_Depth", "Runoff_Volume", 
                                            "Peak_Runoff", "Runoff_Coeff"), 
                                 sep = "_")),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.subcatchment_washoff_summary <- function(x, ...){
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  tidyr::separate(data = x[-c(1:5),],
                  col = "value",
                  into = c("Subcatchment", pollutants),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.node_depth_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:6), ],
                  col = "value",
                  into = c("Node", "Type",
                           "Average_Depth", "Maximum_Depth", "Maximum_HGL", 
                           "Time_of_Max_Occurance_d",
                           "Time_of_Max_Occurance_hm",
                           "Reported_Max_Depth"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.node_inflow_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:7), ],
                  col = "value",
                  into = c("Node", "Type",
                           "Maximum_Lateral_Inflow",
                           "Maximum_Total_Inflow",
                           "Time_of_Max_Occurance_d",
                           "Time_of_Max_Occurance_hm",
                           "Lateral_Inflow_Volume", 
                           "Total_Inflow_Volume", 
                           "Flow_Balance_Error"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.node_flooding_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:8), ],
                  col = "value",
                  into = c("Node", "Hours_Flooded",
                           "Maximum_Rate", 
                           "Time_of_Max_Occurance_d",
                           "Time_of_Max_Occurance_hm",
                           "Total_Flood_Volume", 
                           "Maximum_Ponded_Volume"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outfall_loading_summary <- function(x, ...){
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[5, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.) %>% 
    .[-c(1:4)]
  
  tidyr::separate(data = x[-c(1:6), ],
                  col = "value",
                  into = c("Outfall_Node", "Flow_Freq", "Avg_Flow", "Max_Flow", 
                           "Total_Volume", paste("Total", pollutants, sep = "_")),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.link_flow_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:6), ],
                  col = "value",
                  into = c("Link", "Type", "Maximum_Flow",
                           "Time_of_Max_Occurance_d",
                           "Time_of_Max_Occurance_hm",
                           "Maximum_Veloc",
                           "Maximum_Full_Flow",
                           "Maximum_Full_Depth"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.conduit_surcharge_summary <- function(x, ...){
  
  tidyr::separate(data = x[-c(1:5), ],
                  col = "value",
                  into = c("Conduit",
                           "Hours_Full_Both_Ends", 
                           "Hours_Full_Upstream", 
                           "Hours_Full_Dnstream",
                           "Hours_Above_Full_Normal_Flow", 
                           "Hours_Capacity_Limited"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
  
}

#' import helper
#' @keywords internal
parse_section.link_pollutant_load_summary <- function(x, ...){
  
  # extract pollutants
  pollutants <- gsub("\\W", " ", x[4, ]) %>%
    trimws(.) %>% 
    strsplit(split = "\\s+", x = .) %>%
    unlist(.)
  
  tidyr::separate(data = x[-c(1:5), ],
                  col = "value",
                  into = c("Link", pollutants),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.rpt_error <- function(x, ...){
  
  # ignore the first and last 4 lines (Version and timings)
  tbl <- x[-c(1:4, (nrow(x) - 4):(nrow(x))), ] %>%
    dplyr::filter(. != "") %>% 
    unlist(use.names = FALSE)
  
  tibble::tibble(Error = paste(tbl[seq(1, length(tbl), 2)], 
                               tbl[seq(2, length(tbl), 2)]))
  
}