#' import helper
#' @keywords internal
import_options <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Option", "Value"),
                  sep = c(20), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_title <- function(x) {
  
  x
  
}

#' import helper
#' @keywords internal
import_raingages <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Format", "tab2", "Interval",
                           "tab3", "SCF", "tab4", "Source"),
                  sep = base::cumsum(c(16, 1, 9, 1, 8, 1, 8, 1)), 
                  convert = TRUE)

}

import_hydrographs <- function(x){
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

import_temperature <- function(x) {
  # S.Add('[TEMPERATURE]');
  # S.Add(';;Data Element    ' + Tab + 'Values     ');
  # multiple subsection
}

import_evaporation <- function(x) {
  # S.Add('');
  # S.Add('[EVAPORATION]');
  # Line := ';;Data Source   ' + Tab + 'Parameters';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------';
  # S.Add(Line);
  # multiple subsection
}

import_adjustments <- function(x){
  # S.Add('');
  # S.Add('[ADJUSTMENTS]');
  # Line := ';;Parameter ' + Tab + 'Monthly Adjustments';
  # S.Add(Line);
  # multiple subsection
}

import_events <- function(x) {
  # S.Add('');
  # S.Add('[EVENTS]');
  # S.Add(';;Start Date         End Date');
}

#' import helper
#' @keywords internal
import_subcatchments <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "tab1", "Rain Gage", "tab2",
                           "Outlet", "tab3", "Area", "tab4", 
                           "Perc_Imperv", "tab5", "Width", "tab6",
                           "Perc_Slope", "tab7", "CurbLen", "tab8",
                           "Snowpack"),
                  sep = base::cumsum(c(16, 1, 16, 1, 16, 1,
                                       8, 1, 8, 1, 8, 1, 8, 1, 8, 1)), 
                  convert = TRUE)

}

#' import helper
#' @keywords internal
import_subareas <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "N-Imperv", "tab2",
                           "N-Perv", "tab3", "S-Imperv", "tab4",
                           "S-Perv", "tab5", "PctZero", "tab6", "RouteTo", 
                           "tab7", "PctRouted"),
                  sep = base::cumsum(c(16, 1, 10, 1, 10, 1, 10,
                                       1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_infiltration <- function(x){
  
  # TODO
  # horton
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "MaxRate", "tab2",
                           "MinRate", "tab3", "Decay", "tab4",
                           "DryTime", "tab5", "MaxInfil"),
                  sep = base::cumsum(c(16, 1, 10, 1, 10, 1, 10,
                                       1, 10, 1)), 
                  convert = TRUE)
}

import_aquifers <- function(x){
  # S.Add('');
  # S.Add('[AQUIFERS]');
  # Line := ';;Name          ' + Tab + 'Por   ' + Tab + 'WP    ' + Tab + 'FC    ';
  # Line := Line + Tab + 'Ksat  ' + Tab + 'Kslope' + Tab + 'Tslope' + Tab + 'ETu   '; //(5.1.010)
  # Line := Line + Tab + 'ETs   ' + Tab + 'Seep  ' + Tab + 'Ebot  ' + Tab + 'Egw   '; //(5.1.010)
  # Line := Line + Tab + 'Umc   ' + Tab + 'ETupat ';                                  //(5.1.010)
  # S.Add(Line);
  # Line := ';;--------------';
  # for I := 0 to MAXAQUIFERPROPS do Line := Line + Tab + '------';
  # S.Add(Line);
}

import_groundwater <- function(x){
  # S.Add('');
  # S.Add('[GROUNDWATER]');
  # Line := ';;Subcatchment  ' + Tab + 'Aquifer         ' + Tab + 'Node            ';
  # Line := Line + Tab + 'Esurf ' + Tab + 'A1    ' + Tab + 'B1    ' + Tab + 'A2    ';
  # Line := Line + Tab + 'B2    ' + Tab + 'A3    ' + Tab + 'Dsw   ' + Tab + 'Egwt  '; //(5.1.010)
  # Line := Line + Tab + 'Ebot  ' + Tab + 'Wgr   ' + Tab + 'Umc   ';                  //(5.1.010)
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '------' + Tab + '------' + Tab + '------' + Tab + '------';
  # Line := Line + Tab + '------' + Tab + '------' + Tab + '------' + Tab + '------';
  # Line := Line + Tab + '------' + Tab + '------' + Tab + '------';
  # S.Add(Line);
  # multiple sections
}

import_snowpacks <- function(x){
  # S.Add('');
  # S.Add('[SNOWPACKS]');
  # S.Add(';;Name          ' + Tab + 'Surface   ' + Tab + 'Parameters');
  # S.Add(';;--------------' + Tab + '----------' + Tab + '----------');
  # multiple sections
}

#' import helper
#' @keywords internal
import_junctions <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "Elevation", "tab2", 
                           "MaxDepth", "tab3", "InitDepth", "tab4",
                           "SurDepth", "tab5", "Aponded"),
                  sep = base::cumsum(c(16, 1, 10, 1, 10, 1,  10, 1, 
                                       10, 1)), 
                  convert = TRUE)
  
}

import_outfalls <- function(x) {
  # S.Add('');
  # S.Add('[OUTFALLS]');
  # Line := ';;Name          ' + Tab + 'Elevation ' + Tab + 'Type      ';
  # Line := Line + Tab + 'Stage Data      ' + Tab + 'Gated   ';
  # Line := Line + Tab + 'Route To        ';                                     //(5.1.008)
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  # Line := Line + Tab + '----------------' + Tab + '--------';
  # Line := Line + Tab + '----------------';                                     //(5.1.008)
  # S.Add(Line);
}

import_dividers <- function(x){
  # S.Add('');
  # S.Add('[DIVIDERS]');
  # Line := ';;Name          ' + Tab + 'Elevation ' + Tab + 'Diverted Link   ';
  # Line := Line + Tab + 'Type      ' + Tab + 'Parameters';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------------';
  # Line := Line + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

import_storage <- function(x){
  # S.Add('');
  # S.Add('[STORAGE]');
  # Line := ';;Name          ' + Tab + 'Elev.   ' + Tab + 'MaxDepth  ';
  # Line := Line + Tab + 'InitDepth ' + Tab + 'Shape     ';
  # Line := Line + Tab + 'Curve Name/Params           ';
  # Line := Line + Tab + 'N/A     ' + Tab + 'Fevap   ' + Tab + 'Psi     ';
  # Line := Line + Tab + 'Ksat    ' + Tab + 'IMD     ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '--------' + Tab + '----------';
  # Line := Line + Tab + '-----------' + Tab + '----------';
  # Line := Line + Tab + '----------------------------';
  # Line := Line + Tab + '--------' + Tab + '--------' + Tab + '        ';
  # Line := Line + Tab + '--------' + Tab + '--------';
  # S.Add(Line);
}

#' import helper
#' @keywords internal
import_conduits <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Name", "tab1", "From Node", "tab2", 
                           "To Node", "tab3", "Length", "tab4", 
                           "Roughness", "tab5", "InOffset", "tab6",
                           "OutOffset", "tab7", "InitFlow", "tab8",
                           "MaxFlow"),
                  sep = base::cumsum(c(16, 1, 16, 1, 16, 1,
                                       10, 1, 10, 1, 10, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

import_pumps <- function(x) {
  # S.Add('');
  # S.Add('[PUMPS]');
  # Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  # Line := Line + Tab + 'Pump Curve      ' + Tab + 'Status  ';                  //5.1.007
  # Line := Line + Tab + 'Sartup' + Tab + 'Shutoff ';                            //5.1.007
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '----------------' + Tab + '------';
  # Line := Line + Tab + '--------' + Tab + '--------';
  # S.Add(Line);
}

import_orifices <- function(x) {
  # S.Add('');
  # S.Add('[ORIFICES]');
  # Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  # Line := Line + Tab + 'Type        ' + Tab + 'Offset    ' + Tab + 'Qcoeff    ';
  # Line := Line + Tab + 'Gated   ' + Tab + 'CloseTime ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '------------' + Tab + '----------' + Tab + '----------';
  # Line := Line + Tab + '--------' + Tab + '----------';
  # S.Add(Line);
}

import_weirs <- function(x) {
  # S.Add('');
  # S.Add('[WEIRS]');
  # Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  # Line := Line + Tab + 'Type        ' + Tab + 'CrestHt   ' + Tab + 'Qcoeff    ';
  # Line := Line + Tab + 'Gated   ' + Tab + 'EndCon  ' + Tab + 'EndCoeff  ';
  # Line := Line + Tab + 'Surcharge ';                                           //(5.1.007)
  # 
  # Line := Line + Tab + 'RoadWidth ' + Tab + 'RoadSurf  ';                      //(5.1.010)
  # 
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '------------' + Tab + '----------' + Tab + '----------';
  # Line := Line + Tab + '--------' + Tab + '--------' + Tab + '----------';
  # Line := Line + Tab + '----------';                                           //(5.1.007)
  # 
  # Line := Line + Tab + '----------' + Tab + '----------';                      //(5.1.010)
  # 
  # S.Add(Line);
}

import_outlets <- function(x) {
  # S.Add('');
  # S.Add('[OUTLETS]');
  # Line := ';;Name          ' + Tab + 'From Node       ' + Tab + 'To Node         ';
  # Line := Line + Tab + 'Offset    ' + Tab + 'Type           ';
  # Line := Line + Tab + 'QTable/Qcoeff   ' + Tab + 'Qexpon    ' + Tab + 'Gated   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------------';
  # Line := Line + Tab + '----------' + Tab + '---------------';
  # Line := Line + Tab + '----------------' + Tab + '----------' + Tab + '--------';
  # S.Add(Line);
}

import_xsections <- function(x) {
  # S.Add('');
  # S.Add('[XSECTIONS]');
  # Line := ';;Link          ' + Tab + 'Shape       ' + Tab + 'Geom1           ';
  # Line := Line + Tab + 'Geom2     ' + Tab + 'Geom3     ' + Tab + 'Geom4     ';
  # Line := Line + Tab + 'Barrels   ' + Tab + 'Culvert   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '------------' + Tab + '----------------';
  # Line := Line + Tab + '----------' + Tab + '----------' + Tab + '----------';
  # Line := Line + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

import_transects <- function(x) {
  # S.Add('');
  # S.Add('[TRANSECTS]');
  # S.Add(';;Transect Data in HEC-2 format');                                    //5.1.008)
  # multiple options
}

import_losses <- function(x){
  # S.Add('');
  # S.Add('[LOSSES]');
  # Line := ';;Link          ' + Tab + 'Kentry    ' + Tab + 'Kexit     ';
  # Line := Line + Tab + 'Kavg      ' + Tab + 'Flap Gate ' + Tab + 'Seepage   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------';
  # Line := Line + tab + '----------' + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

#' import helper
#' @keywords internal
import_pollutants <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "tab1", "Units", "tab2",
                           "Crain", "tab3", "Cgw", "tab4",
                           "Crdii", "tab5", "Kdecay", "tab6",
                           "SnowOnly", "tab7", "Co-Pollutant", "tab8", 
                           "Co-Frac", "tab9", "Cdwf", "tab10", "Cinit"),
                  sep = base::cumsum(c(16, 1, 6, 1, 10, 1, 10, 1,
                                       10, 1, 10, 1, 10, 1, 16, 1,
                                       10, 1, 10, 1, 10)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_landuses <- function(x){
  
  tidyr::separate(data = x,
                  col = "value", 
                  into = c("Name", "tab1", "Sweeping_Interval", "tab2",
                           "Fraction_Available", "tab3", "Last_Swept"),
                  sep = base::cumsum(c(16, 1, 10, 1, 10, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_buildup <- function(x){
  
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
import_washoff <- function(x){
  
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

import_coverage <- function(x) {
  # S.Add('');
  # S.Add('[COVERAGES]');
  # Line := ';;Subcatchment  ' + Tab + 'Land Use        ' + Tab + 'Percent   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # S.Add(Line);
}

import_loadings <- function(x) {
  # S.Add('');
  # S.Add('[LOADINGS]');
  # Line := ';;Subcatchment  ' + Tab + 'Pollutant       ' + Tab + 'Buildup   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # S.Add(Line);
}

import_treatment <- function(x) {
  # S.Add('');
  # S.Add('[TREATMENT]');
  # Line := ';;Node          ' + Tab + 'Pollutant       ' + Tab + 'Function  ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # S.Add(Line);
}

import_inflows <- function(x) {
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

import_dwflows <- function(x) {
  # S.Add('');
  # S.Add('[DWF]');
  # Line := ';;Node          ' + Tab + 'Constituent     ' + Tab + 'Baseline  ';
  # Line := Line + Tab + 'Patterns  ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # Line := Line + Tab + '----------';
  # S.Add(Line);
}

import_iiflows <- function(x) {
  # S.Add('');
  # S.Add('[RDII]');
  # Line := ';;Node          ' + Tab + 'Unit Hydrograph ' + Tab + 'Sewer Area';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------------' + Tab + '----------';
  # S.Add(Line);
}

import_patterns <- function(x) {
  # S.Add('');
  # S.Add('[PATTERNS]');
  # Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'Multipliers';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '-----------';
  # S.Add(Line);
}

import_timeseries <- function(x) {
  # S.Add('');
  # S.Add('[TIMESERIES]');
  # Line := ';;Name          ' + Tab + 'Date      ' + Tab + 'Time      ' + Tab + 'Value     ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

import_curves <- function(x){
  # S.Add('');
  # S.Add('[CURVES]');
  # Line := ';;Name          ' + Tab + 'Type      ' + Tab + 'X-Value   ' + Tab + 'Y-Value   ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------' + Tab + '----------' + Tab + '----------';
  # S.Add(Line);
}

import_controls <- function(x) {
  # S.Add('');
  # S.Add('[CONTROLS]');
}

import_report <- function(x) {
  # S.Add('');
  # S.Add('[REPORT]');
  # S.Add(';;Reporting Options');
  # S.Add('INPUT     ' + Tab + Project.Options.Data[REPORT_INPUT_INDEX]);
  # S.Add('CONTROLS  ' + Tab + Project.Options.Data[REPORT_CONTROLS_INDEX]);
}

import_files <- function(x) {
  # S.Add('');
  # S.Add('[FILES]');
  # S.Add(';;Interfacing Files');
  # TokList := TStringList.Create;
}

import_profiles <- function(x) {
  # S.Add('');
  # S.Add('[PROFILES]');
  # Line := ';;Name          ' + Tab + 'Links     ';
  # S.Add(Line);
  # Line := ';;--------------' + Tab + '----------';
}

import_tags <- function(x) {
  # S.Add('');
  # S.Add('[TAGS]');
}

import_map <- function(x) {
  # S.Add('');
  # S.Add('[MAP]');
}

#' import helper
#' @keywords internal
import_coordinates <- function(x){
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Node", "tab1", "X-Coord", "tab2", "Y-Coord"),
                  sep = base::cumsum(c(16, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_vertices <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Link", "tab1", "X-Coord", "tab2", "Y-Coord"),
                  sep = base::cumsum(c(16, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_polygons <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Subcatchment", "tab1", "X-Coord", "tab2", "Y-Coord"),
                  sep = base::cumsum(c(16, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_symbols <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("Gage", "tab1", "X-Coord", "tab2", "Y-Coord"),
                  sep = base::cumsum(c(16, 1, 18, 1)), 
                  convert = TRUE)
  
}

#' import helper
#' @keywords internal
import_labels <- function(x) {
  
  tidyr::separate(data = x, 
                  col = "value", 
                  into = c("X-Coord", "tab1", "Y-Coord", "tab2", "Label"),
                  sep = base::cumsum(c(18, 1, 18, 1)), 
                  convert = TRUE)
  
}

import_backdrop <- function(x) {
  # S.Add('');
  # S.Add('[BACKDROP]');
  # S.Add('FILE      ' + Tab + '"' + Filename + '"');
  # Line := 'DIMENSIONS' + Tab +
  #   FloatToStrF(LowerLeft.X,ffFixed,18,D) + Tab +
  #   FloatToStrF(LowerLeft.Y,ffFixed,18,D) + Tab +
  #   FloatToStrF(UpperRight.X,ffFixed,18,D) + Tab +
  #   FloatToStrF(UpperRight.Y,ffFixed,18,D);
  # S.Add(Line);
}