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
                  into = c("Name", "Format", "Interval",
                           "SCF", "Source"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
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
                  into = c("Data Source", "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
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
parse_section.subareas <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Subcatchment",
                           "N-Imperv", 
                           "N-Perv", "S-Imperv",
                           "S-Perv", "PctZero", "RouteTo", 
                           "PctRouted"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.infiltration <- function(x){
  
  # TODO
  # horton
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment","MaxRate", 
                           "MinRate", "Decay",
                           "DryTime", "MaxInfil"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)

}

#' import helper
#' @keywords internal
parse_section.aquifers <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Por", 
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
parse_section.snowpacks <- function(x){
  
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
parse_section.junctions <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Elevation", 
                           "MaxDepth", "InitDepth", 
                           "SurDepth", "Aponded"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outfalls <- function(x) {
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Elevation", 
                           "Type", "Stage Data", 
                           "Gated", "Route To"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.dividers <- function(x){
 
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
parse_section.storage <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name","Elev.", 
                           "MaxDepth", "InitDepth", 
                           "Shape", "Curve Name/Params", 
                           "N/A", "Fevap", 
                           "Psi", "Ksat", "IMD"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.conduits <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Length", 
                           "Roughness", "InOffset", 
                           "OutOffset", "InitFlow", 
                           "MaxFlow"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.pumps <- function(x) {
  
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
parse_section.orifices <- function(x) {
  
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
parse_section.weirs <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "From Node", 
                           "To Node", "Type", 
                           "CrestHt", "Qcoeff", 
                           "Gated", "EndCon", 
                           "EndCoeff", "Surcharge",
                           "RoadWidth", "RoadSurf"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.outlets <- function(x) {
  
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
parse_section.xsections <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "Shape", 
                           "Geom1", "Geom2", 
                           "Geom3", "Geom4", 
                           "Barrels", "Culvert"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.losses <- function(x){
  
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
parse_section.pollutants <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Units", 
                           "Crain", "Cgw", 
                           "Crdii", "Kdecay", 
                           "SnowOnly", "Co-Pollutant", 
                           "Co-Frac", "Cdwf", "Cinit"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.landuses <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "Sweeping_Interval", 
                           "Fraction_Available", "Last_Swept"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.buildup <- function(x){
  
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
parse_section.washoff <- function(x){
  
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
parse_section.coverages <- function(x) {
  
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
parse_section.loadings <- function(x) {
  
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
parse_section.treatment <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "Pollutant", 
                           "Function"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
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
                  into = c("object", "id", "text"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
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
                  into = c("Node", "X-Coord", "Y-Coord"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.vertices <- function(x) {
  
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
parse_section.polygons <- function(x) {
  
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
parse_section.symbols <- function(x) {
  
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
parse_section.labels <- function(x) {
  
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
parse_section.lidcontrols <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "Type/Layer", "Parameters"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
parse_section.lidusage <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "LID Process", 
                           "Number", "Area", 
                           "Width", "InitSat", 
                           "FromImp", "ToPerv", 
                           "RptFile", "DrainTo"),
                  sep = "\\s+",
                  extra = "merge",
                  fill = "left",
                  convert = TRUE)

}