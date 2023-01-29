section_columns <- function(section_name = NULL)
{
  columns_per_section <- list(
    
    aquifers = c(
      "Por", 
      "WP", 
      "FC", 
      "Ksat", 
      "Kslope", 
      "Tslope", 
      "ETu", 
      "ETs", 
      "Seep", 
      "Ebot", 
      "Egw", 
      "Umc", 
      "ETupat"
    ),
    
    buildup = c(
      "LandUse", 
      "Pollutant", 
      "Function", 
      "Coeff1", 
      "Coeff2", 
      "Coeff3", 
      "Per_Unit"
    ),
    
    conduits = c(
      "Name", 
      "From Node", 
      "To Node", 
      "Length", 
      "Roughness", 
      "InOffset", 
      "OutOffset", 
      "InitFlow", 
      "MaxFlow"
    ),
    
    conduit_surcharge_summary = c(
      "Conduit", 
      "Hours_Full_Both_Ends", 
      "Hours_Full_Upstream", 
      "Hours_Full_Dnstream", 
      "Hours_Above_Full_Normal_Flow", 
      "Hours_Capacity_Limited"
    ),
    
    coverages = c(
      "Subcatchment", 
      "Land Use", 
      "Percent"
    ),
    
    cross_section_summary = c(
      "Conduit", 
      "Shape", 
      "Full_Depth", 
      "Full_Area", 
      "Hyd_Rad", 
      "Max_Width", 
      "No_of_Barrels", 
      "Full_Flow"
    ),
    
    curves = c(
      "Type", 
      "X-Value", 
      "Y-Value"
    ),
    
    dividers = c(
      "Name", 
      "Elevation", 
      "Diverted Link", 
      "Type", 
      "Parameters"
    ),
    
    dwf = c(
      "Node", 
      "Constituent", 
      "Baseline", 
      "Patterns"
    ),
    
    evaporation = c(
      "Data Source", 
      "Parameters"
    ),
    
    events = c(
      "Start Date", 
      "End Date"
    ),
    
    flow_classification_summary = c(
      "Conduit", 
      "Adjusted_Actual_Length", 
      "Dry", 
      "Up_Dry", 
      "Down_Dry", 
      "Sub_Crit", 
      "Sup_Crit", 
      "Up_Crit", 
      "Down_Crit", 
      "Norm_Ltd", 
      "Inlet_Ctrl"
    ),
    
    groundwater = c(
      "Aquifer", 
      "Node", 
      "Esurf", 
      "A1", 
      "B1", 
      "A2", 
      "B2", 
      "A3", 
      "Dsw", 
      "Egwt",
      "Ebot", 
      "Wgr", 
      "Umc"
    ),
    
    groundwater_summary = c(
      "Subcatchment", 
      "Total_Infil", 
      "Total_Evap", 
      "Total_Lower_Seepage", 
      "Total_Lateral_Outflow", 
      "Maximum_Lateral_Outflow", 
      "Average_Upper_Moist", 
      "Average_Water_Table", 
      "Final_Upper_Moist", 
      "Final_Water_Table"
    ),
    
    hydrographs = c(
      "Hydrograph", 
      "Rain Gage/Month", 
      "Response", 
      "R", 
      "T", 
      "K", 
      "Dmax", 
      "Drecov", 
      "Dinit"
    ),
    
    iiflows = c(
      "Node", 
      "Unit Hydrograph", 
      "Sewer Area"
    ),
    
    inflows = c(
      "Constituent", 
      "Time Series", 
      "Type", 
      "Mfactor", 
      "Sfactor", 
      "BaseLine", 
      "Pattern"
    ),
    
    junctions = c(
      "Name", 
      "Elevation", 
      "MaxDepth", 
      "InitDepth", 
      "SurDepth", 
      "Aponded"
    ),
    
    landuses = c(
      "Sweeping_Interval", 
      "Fraction_Available", 
      "Last_Swept"
    ),
    
    landuse_summary = c(
      "Name", 
      "Sweeping_Interval", 
      "Maximum_Removal", 
      "Last_Swept"
    ),
    
    lid_control_summary = c(
      "Subcatchment", 
      "LID_Control", 
      "No_of_Units", 
      "Unit_Area", 
      "Unit_Width", 
      "Percent_Area_Covered",
      "Percent_Imperv_Treated", 
      "Percent_Treated"
    ),
    
    lid_performance_summary = c(
      "Subcatchment",
      "LID Control", 
      "Total_Inflow",
      "Evap_Loss",
      "Infil_Loss",
      "Surface_Outflow",
      "Drain_Outflow",
      "Initial_Storage", 
      "Final_Storage",
      "Continuity_Error"
    ),
    
    lid_usage = c(
      "Subcatchment", 
      "LID Process",
      "Number", 
      "Area", 
      "Width", 
      "InitSat", 
      "FromImp", 
      "ToPerv", 
      "RptFile", 
      "DrainTo"
    ),
    
    link_flow_summary = c(
      "Link", 
      "Type", 
      "Maximum_Flow", 
      "Time_of_Max_Occurance_d", 
      "Time_of_Max_Occurance_hm", 
      "Maximum_Veloc", 
      "Maximum_Full_Flow", 
      "Maximum_Full_Depth"
    ), 
    
    link_summary = c(
      "Name", 
      "From Node", 
      "To Node", 
      "Type", 
      "Length", 
      "Perc_Slope", 
      "Roughness"
    ),
    
    loadings = c(
      "Subcatchment", 
      "Pollutant", 
      "Buildup"
    ),
    
    losses = c(
      "Link", 
      "Kentry", 
      "Kexit", 
      "Kavg", 
      "Flap Gate", 
      "Seepage"
    ),
    
    node_depth_summary = c(
      "Node", 
      "Type", 
      "Average_Depth", 
      "Maximum_Depth", 
      "Maximum_HGL", 
      "Time_of_Max_Occurance_d", 
      "Time_of_Max_Occurance_hm", 
      "Reported_Max_Depth"
    ),
    
    node_flooding_summary = c(
      "Node", 
      "Hours_Flooded", 
      "Maximum_Rate", 
      "Time_of_Max_Occurance_d", 
      "Time_of_Max_Occurance_hm", 
      "Total_Flood_Volume", 
      "Maximum_Ponded_Volume"
    ),
    
    node_inflow_summary = c(
      "Node", 
      "Type", 
      "Maximum_Lateral_Inflow", 
      "Maximum_Total_Inflow",
      "Time_of_Max_Occurance_d", 
      "Time_of_Max_Occurance_hm", 
      "Lateral_Inflow_Volume", 
      "Total_Inflow_Volume", 
      "Flow_Balance_Error"
    ),
    
    node_summary = c(
      "Name", 
      "Type", 
      "Invert_Elev", 
      "Max_Depth", 
      "Ponded_Area", 
      "External_Inflow"
    ),
    
    node_surcharge_summary = c(
      "Node", 
      "Type", 
      "Hours_Surcharged", 
      "Max_Height_Above_Crown_Feet",
      "Min_Depth_Below_Rim_Feet"
    ),
    
    options = c(
      "Option", 
      "Value"
    ),
      
    orifices = c(
      "Name", 
      "From Node", 
      "To Node", 
      "Type", 
      "Offset", 
      "Qcoeff", 
      "Gated", 
      "CloseTime"
    ),
    
    outfalls = c(
      "Elevation", 
      "tab2", 
      "Type", 
      "tab3", 
      "Stage Data", 
      "tab4", 
      "Gated", 
      "tab5", 
      "Route To"
    ),
    
    outlets = c(
      "Name", 
      "From Node", 
      "To Node", 
      "Offset", 
      "Type", 
      "QTable/Qcoeff", 
      "Qexpon", 
      "Gated"
    ),
    
    patterns = c(
      "Name", 
      "Type", 
      "Multipliers"
    ),
    
    pollutants = c(
      "Name", 
      "Units", 
      "Crain", 
      "Cgw", 
      "Crdii", 
      "Kdecay", 
      "SnowOnly", 
      "Co-Pollutant", 
      "Co-Frac", 
      "Cdwf", 
      "Cinit"
    ),
    
    pollutant_summary = c(
      "Name", 
      "Units", 
      "Ppt_Concen", 
      "GW_Concen", 
      "Kdecay_per_day", 
      "CoPollutant"
    ),
    
    pumps = c(
      "Name", 
      "From Node", 
      "To Node", 
      "Pump Curve", 
      "Status", 
      "Sartup", 
      "Shutoff"
    ),
    
    pumping_summary = c(
      "Pump", 
      "Percent_Utilized", 
      "Number_of_Start_Ups", # "Number_of_StartUps"
      "Min_Flow", 
      "Avg_Flow", 
      "Max_Flow", 
      "Total_Volume", 
      "Power_Usage", 
      "Time_Off_Pump_Curve_Low", 
      "Time_Off_Pump_Curve_High"
    ),
    
    raingages = c(
      "Name", 
      "Format", 
      "Interval", 
      "SCF", 
      "Source"
    ),
    
    raingage_summary = c(
      "Name", 
      "Data_Source", 
      "Data_Type", 
      "Recording_Interval"
    ),
    
    snowpacks = c(
      "Name", 
      "Surface", 
      "Parameters"
    ),
    
    storage = c(
      "Elev.", 
      "MaxDepth", 
      "InitDepth", 
      "Shape", 
      "Curve Name/Params", 
      "N/A", 
      "Fevap", 
      "Psi", 
      "Ksat", 
      "IMD"
    ),
    
    storage_volume_summary = c(
      "Storage_Unit", 
      "Average_Volume", 
      "Avg_Pcnt_Full", 
      "Evap_Pcnt_Loss", 
      "Exfil_Pcnt_Loss", 
      "Maximum_Volume", 
      "Max_Pcnt_Full", 
      "Time_of_Max_Occurence_days", 
      "Time_of_Max_Occurence_hr_min",
      "Maximum_Outflow"
    ),
    
    subareas = c(
      "N-Imperv", 
      "N-Perv", 
      "S-Imperv", 
      "S-Perv", 
      "PctZero", 
      "RouteTo", 
      "PctRouted"
    ),
    
    subcatchments = c(
      "Name", 
      "Rain Gage", 
      "Outlet", 
      "Area",
      "Perc_Imperv", 
      "Width", 
      "Perc_Slope", 
      "CurbLen", 
      "Snowpack"
    ),
    
    subcatchment_runoff_summary = c(
      "Subcatchment", 
      paste("Total", sep = "_", c(
        "Precip", 
        "Runon", 
        "Evap", 
        "Infil", 
        "Runoff_imperv_Depth", 
        "Runoff_perv_Depth",
        "Runoff_Depth", 
        "Runoff_Volume", 
        "Peak_Runoff", 
        "Runoff_Coeff"
      ))
    ),
    
    subcatchment_summary = c(
      "Name", 
      "Area", 
      "Width", 
      "Perc_Imperv", 
      "Perc_Slope", 
      "Rain_Gage", 
      "Outlet"
    ),
    
    temperature = c(
      "Data Element", 
      "tab1", 
      "Values"
    ),
    
    timeseries = c(
      "Date", 
      "Time", 
      "Value"
    ),
    
    treatment = c(
      "Node", 
      "Pollutant", 
      "Function"
    ),
    
    washoff = c(
      "LandUse", 
      "Pollutant", 
      "Function", 
      "Coeff1", 
      "Coeff2", 
      "SweepRmvl", 
      "BmpRmvl"
    ),
    
    weirs = c(
      "From Node", 
      "To Node", 
      "Type", 
      "CrestHt", 
      "Qcoeff", 
      "Gated", 
      "EndCon", 
      "EndCoeff", 
      "Surcharge", 
      "RoadWidth", 
      "RoadSurf"
    ),
    
    xsections = c(
      "Shape", 
      "Geom1", 
      "Geom2", 
      "Geom3", 
      "Geom4", 
      "Barrels", 
      "Culvert"
    )
    
  )
  
  cps2 <- system_file("extdata/config/columns.csv") %>%
    read.csv() %>%
    split(factor(.$section, unique(.$section))) %>%
    lapply("[[", "column")

  stopifnot(identical(columns_per_section, cps2))
  
  if (is.null(section_name)) {
    return(columns_per_section)
  }
  
  if (!section_name %in% names(columns_per_section)) {
    stop_formatted(
      "No columns defined for section '%s' in section_columns()", 
      section_name
    )
  }
  
  columns_per_section[[section_name]]
}
