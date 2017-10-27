#' import helper
#' @keywords internal
parse_section <- function(x) {
  UseMethod("parse_section", x)
} 

#' import helper
#' @keywords internal
parse_section.default <- function(x) {
  warning(paste("Unknown class:", tail(class(x), 1)))
  return(NULL)
}

#' import helper
#' @keywords internal
parse_section.options <- function(x){
  
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
parse_section.title <- function(x) {
  
  x
  
}

#' import helper
#' @keywords internal
parse_section.raingages <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Format", "tab2", "Interval",
                           "tab3", "SCF", "tab4", "Source"),
                  sep = base::cumsum(c(16, 1, 9, 1, 8, 1, 8, 1)), 
                  convert = TRUE)

}

parse_section.hydrographs <- function(x){
  # S.Add('[HYDROGRAPHS]');
  # Line := ';;Hydrograph    ' + Tab + 'Rain Gage/Month ' + Tab + 'Response';
  # Line := Line + Tab + 'R       ' + Tab + 'T       ' + Tab + 'K       ';
  # Line := Line + Tab + 'Dmax    ' + Tab + 'Drecov  ' + Tab + 'Dinit   ';       //(5.1.007)
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '--------';
  # Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  # Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  # S.Add(Line);
}

parse_section.temperature <- function(x) {
  # S.Add('[TEMPERATURE]');
  # S.Add(';;Data Element    ' + Tab + 'Values     ');
  # multiple subsection
}

#' import helper
#' @keywords internal
parse_section.evaporation <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Data Source", "tab1", "Parameters"),
                  sep = base::cumsum(c(16, 1)), 
                  convert = TRUE)
  
}

parse_section.events <- function(x) {
  # S.Add('');
  # S.Add('[EVENTS]');
  # S.Add(';;Start Date         End Date');
}

#' import helper
#' @keywords internal
parse_section.subcatchments <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Rain Gage", "tab2",
                           "Outlet", "tab3", "Area", "tab4", 
                           "Perc_Imperv", "tab5", "Width", "tab6",
                           "Perc_Slope", "tab7", "CurbLen", "tab8",
                           "Snowpack"),
                  sep = base::cumsum(c(16, 1, 16, 1,
                                       8, 1, 8, 1, 8, 1, 8, 1, 8, 1)), 
                  convert = TRUE)

}

#' import helper
#' @keywords internal
parse_section.subareas <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>%
  tidyr::separate(col = "value", 
                  into = c("N-Imperv", "tab2",
                           "N-Perv", "tab3", "S-Imperv", "tab4",
                           "S-Perv", "tab5", "PctZero", "tab6", "RouteTo", 
                           "tab7", "PctRouted"),
                  sep = base::cumsum(c(10, 1, 10, 1, 10,
                                       1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.infiltration <- function(x){
  
  # TODO
  # horton
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>%
  tidyr::separate(col = "value", 
                  into = c("MaxRate", "tab2",
                           "MinRate", "tab3", "Decay", "tab4",
                           "DryTime", "tab5", "MaxInfil"),
                  sep = base::cumsum(c(10, 1, 10, 1, 10,
                                       1, 10, 1)), 
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.aquifers <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Por", "tab2", 
                           "WP", "tab3", "FC", "tab4",
                           "Ksat", "tab5", "Kslope", "tab6",
                           "Tslope", "tab7", "ETu", "tab8",
                           "ETs", "tab9", "Seep", "tab10",
                           "Ebot", "tab11", "Egw", "tab12",
                           "Umc", "tab13", "ETupat"),
                  sep = base::cumsum(c(16, 1, 6, 1, 6, 1, 6, 1, 
                                       6, 1, 6, 1, 6, 1, 6, 1, 6, 1,
                                       6, 1, 6, 1, 6, 1, 6, 1)), 
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.snowpacks <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Surface", "tab2", 
                           "Parameters"),
                  sep = base::cumsum(c(16, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.junctions <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Elevation", "tab2", 
                           "MaxDepth", "tab3", "InitDepth", "tab4",
                           "SurDepth", "tab5", "Aponded"),
                  sep = base::cumsum(c(10, 1, 10, 1,  10, 1, 
                                       10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outfalls <- function(x) {
  
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
                  sep = base::cumsum(c(10, 1, 10, 1,  10, 1, 
                                       10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.dividers <- function(x){
 
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Elevation", "tab2", 
                           "Diverted Link", "tab3", "Type", "tab4",
                           "Parameters"),
                  sep = base::cumsum(c(16, 1, 10, 1, 16, 1,  10, 1)), 
                  convert = TRUE) 
  
}

#' import helper
#' @keywords internal
parse_section.storage <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Elev.", "tab2", 
                           "MaxDepth", "tab3", "InitDepth", "tab4", 
                           "Shape", "tab5", "Curve Name/Params", "tab6",
                           "N/A", "tab7", "Fevap", "tab8",
                           "Psi", "tab9", "Ksat", "tab10", "IMD"),
                  sep = base::cumsum(c(8, 1, 10, 1, 10, 1, 10, 1, 
                                       28, 1, 8, 1,
                                       8, 1, 8, 1, 8, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.conduits <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", "tab2", 
                           "To Node", "tab3", "Length", "tab4", 
                           "Roughness", "tab5", "InOffset", "tab6",
                           "OutOffset", "tab7", "InitFlow", "tab8",
                           "MaxFlow"),
                  sep = base::cumsum(c(16, 1, 16, 1,
                                       10, 1, 10, 1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.pumps <- function(x) {
  
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", "tab2", 
                           "To Node", "tab3", "Pump Curve", "tab4", 
                           "Status", "tab5", "Sartup", "tab6",
                           "Shutoff"),
                  sep = base::cumsum(c(16, 1, 16, 1, 16, 1,
                                       8, 1, 8, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.orifices <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", "tab2", 
                           "To Node", "tab3", "Type", "tab4", 
                           "Offset", "tab5", "Qcoeff", "tab6",
                           "Gated", "tab7", "CloseTime"),
                  sep = base::cumsum(c(16, 1, 16, 1, 12, 1,
                                       10, 1, 10, 1, 8, 1)), 
                  convert = TRUE)
}

#' import helper
#' @keywords internal
parse_section.weirs <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", "tab2", 
                           "To Node", "tab3", "Type", "tab4", 
                           "CrestHt", "tab5", "Qcoeff", "tab6",
                           "Gated", "tab7", "EndCon", "tab8",
                           "EndCoeff", "tab9", "Surcharge", "tab10",
                           "RoadWidth", "tab11", "RoadSurf"),
                  sep = base::cumsum(c(16, 1, 16, 1,
                                       12, 1, 10, 1, 10, 1,
                                       8, 1, 8, 1, 10, 1,
                                      10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outlets <- function(x) {
  
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("From Node", "tab2", 
                           "To Node", "tab3", "Offset", "tab4", 
                           "Type", "tab5", "QTable/Qcoeff", "tab6",
                           "Qexpon", "tab7", "Gated"),
                  sep = base::cumsum(c(16, 1, 16, 1,
                                       10, 1, 16, 1, 16, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.xsections <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Shape", "tab2", 
                           "Geom1", "tab3", "Geom2", "tab4", 
                           "Geom3", "tab5", "Geom4", "tab6",
                           "Barrels", "tab7", "Culvert"),
                  sep = base::cumsum(c(12, 1, 16, 1,
                                       10, 1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.losses <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("Kentry", "tab2", 
                           "Kexit", "tab3", "Kavg", "tab4", 
                           "Flap Gate", "tab5", "Seepage"),
                  sep = base::cumsum(c(10, 1, 10, 1,
                                       10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.pollutants <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "tab1", "Units", "tab2",
                           "Crain", "tab3", "Cgw", "tab4",
                           "Crdii", "tab5", "Kdecay", "tab6",
                           "SnowOnly", "tab7", "Co-Pollutant", "tab8", 
                           "Co-Frac", "tab9", "Cdwf", "tab10", "Cinit"),
                  sep = base::cumsum(c(16, 1, 6, 1, 10, 1, 10, 1,
                                       10, 1, 10, 1, 10, 1, 16, 1,
                                       10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.landuses <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "tab1", "Sweeping_Interval", "tab2",
                           "Fraction_Available", "tab3", "Last_Swept"),
                  sep = base::cumsum(c(16, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.buildup <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("LandUse", "tab1", "Pollutant", "tab2",
                           "Function", "tab3", "Coeff1", "tab4",
                           "Coeff2", "tab5", "Coeff3", "tab6", 
                           "Per_Unit"),
                  sep = base::cumsum(c(16, 1, 16, 1, 
                                       10, 1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.washoff <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("LandUse", "tab1", "Pollutant", "tab2",
                           "Function", "tab3", "Coeff1", "tab4",
                           "Coeff2", "tab5", "SweepRmvl", "tab6", 
                           "BmpRmvl"),
                  sep = base::cumsum(c(16, 1, 16, 1, 
                                       10, 1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.coverages <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "Land Use", "tab2",
                           "Percent"),
                  sep = base::cumsum(c(16, 1, 16, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.loadings <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "Pollutant", "tab2",
                           "Buildup"),
                  sep = base::cumsum(c(16, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.treatment <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "tab1", "Pollutant", "tab2",
                           "Function"),
                  sep = base::cumsum(c(16, 1, 16, 1)), 
                  convert = TRUE)

}

parse_section.inflows <- function(x) {
  # S.Add('');
  # S.Add('[INFLOWS]');
  # Line := ';;Node          ' + Tab + 'Constituent     ' + Tab + 'Time Series     ';
  # Line := Line + Tab + 'Type    ' + Tab + 'Mfactor ' + Tab + 'Sfactor ';
  # Line := Line + Tab + 'Baseline' + Tab + 'Pattern';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '--------' + Tab + '--------' + Tab + '--------';
  # Line := Line + Tab + '--------' + Tab + '--------';
  # S.Add(Line);
}

parse_section.dwflows <- function(x) {
  # S.Add('');
  # S.Add('[DWF]');
  # Line := ';;Node          ' + Tab + 'Constituent     ' + Tab + 'Baseline  ';
  # Line := Line + Tab + 'Patterns  ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # Line := Line + Tab + '----------';
  # S.Add(Line);
}

parse_section.iiflows <- function(x) {
  # S.Add('');
  # S.Add('[RDII]');
  # Line := ';;Node          ' + Tab + 'Unit Hydrograph ' + Tab + 'Sewer Area';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # S.Add(Line);
}

parse_section.patterns <- function(x) {
  # S.Add('');
  # S.Add('[PATTERNS]');
  # Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'Multipliers';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '-----------';
  # S.Add(Line);
}

parse_section.timeseries <- function(x) {
  # S.Add('');
  # S.Add('[TIMESERIES]');
  # Line := ';;Name          ' + Tab + 'Date      ' + Tab + 'Time      ' + Tab + 'Value     ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

parse_section.curves <- function(x){
  # S.Add('');
  # S.Add('[CURVES]');
  # Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'X-Value   ' + Tab + 'Y-Value   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

parse_section.controls <- function(x) {
  # S.Add('');
  # S.Add('[CONTROLS]');
}

#' import helper
#' @keywords internal
parse_section.report <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Reporting Options", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)

}

parse_section.files <- function(x) {
  # S.Add('');
  # S.Add('[FILES]');
  # S.Add(';;Interfacing Files');
  # TokList := TStringList.Create;
}

parse_section.profiles <- function(x) {
  # S.Add('');
  # S.Add('[PROFILES]');
  # Line := ';;Name          ' + Tab + 'Links     ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------';
}

#' import helper
#' @keywords internal
parse_section.tags <- function(x) {
  
  # warning: is there TAB missing for writing subcatchments in Uexport.pas?  
  # thus, we need a two step procedure
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("object", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
    tidyr::separate(col = "value", 
                    into = c("id", "tab1", "text"),
                    sep = base::cumsum(c(16, 1)), 
                    convert = TRUE)
    
}

#' import helper
#' @keywords internal
parse_section.map <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("key", "value"),
                  extra = "merge",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.coordinates <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("X-Coord", "tab1", "Y-Coord"),
                  sep = base::cumsum(c(18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.vertices <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("X-Coord", "tab1", "Y-Coord"),
                  sep = base::cumsum(c(18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.polygons <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "value"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE) %>% 
  tidyr::separate(col = "value", 
                  into = c("X-Coord", "tab1", "Y-Coord"),
                  sep = base::cumsum(c(18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.symbols <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Gage", "tab1", "X-Coord", "tab2", "Y-Coord"),
                  sep = base::cumsum(c(16, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.labels <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("X-Coord", "tab1", "Y-Coord", "tab2", "Label"),
                  sep = base::cumsum(c(18, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.lidcontrols <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Type/Layer", "tab2", "Parameters"),
                  sep = base::cumsum(c(16, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.lidusage <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "LID Process", "tab2",
                           "Number", "tab3", "Area", "tab4",
                           "Width", "tab5", "InitSat", "tab6",
                           "FromImp", "tab7", "ToPerv", "tab8",
                           "RptFile", "tab9", "DrainTo"),
                  sep = base::cumsum(c(16, 1, 16, 1, 7, 1,
                                       10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 
                                       24, 1)), 
                  convert = TRUE)

}