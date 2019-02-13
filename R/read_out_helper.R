#' Get the type of SWMM element
#' @keywords internal
.get_iType <- function(iType = NULL) {

  choices <- c("subcatchment", "node", "link", "system variable")

  # iType must be NULL or an integer vector of length one  
  if (! is.null(iType)) {
    stop_on_bad_index(iType, choices)
  } 

  if (is.null(iType)) {
    selection <- utils::select.list(choices = choices, multiple = FALSE)
    if (selection == "") {
      message("Nothing was selected.")
      return(NULL)
    }
    iType <- match(selection, choices) -1
  }
  
  # create list with numeric vector with base 0 and var names
  list(iType = iType, names = choices[iType + 1])
}

#' Get the names of SWMM elements
#' @keywords internal
.get_iIndex <- function(list_of_elements, iType = NULL, object_name = NULL) {

  # subcatchments
  if (iType == 0 || iType == 1 || iType == 2) {
    
    element <- c("subcatchments", "nodes", "links")[iType + 1]
    
    object_names <- list_of_elements[[element]]$names

    if (length(object_names) > 0) {
      
      if (is.null(object_name)) {
        object_name <- utils::select.list(object_names, multiple = TRUE)
      } 

      result <- list(iIndex = match(object_name, object_names) - 1,
                     names = object_name)

    } else {
      
      warning("no ", element)
    }    

  } else {
    
    warning("Unclear iType.")
  }
  
  if (anyNA(result$iIndex)) clean_stop("Unclear iIndex.")
  
  if (identical(result$iIndex, numeric(0))) clean_stop("Unclear iIndex.")
  
  result
}

#' Get the simulated values
#' @keywords internal
.get_vIndex <- function(iType, vIndex = NULL, PollNames = NULL) {
  
  type_choices <- list(
    
    subcatchments = c(
      "rainfall rate (in/hr or mm/hr)",
      "snow depth (inches or millimeters)",
      "evaporation loss (in/day or mm/day)",
      "infiltration loss (in/hr or mm/hr)",
      "runoff flow (flow units)",
      "groundwater flow into the drainage network (flow units)",
      "groundwater elevation (ft or m)",
      "soil moisture in the unsaturated groundwater zone (volume fraction)"
    ),
    
    nodes = c(
      "water depth (ft or m above the node invert elevation)",
      "hydraulic head (ft or m, absolute elevation per vertical datum)",
      "stored water volume (including ponded water, ft3 or m3)",
      "lateral inflow (runoff + all other external inflows, in flow units)",
      "total inflow (lateral inflow + upstream inflows, in flow units)",
      "surface flooding (excess overflow when the node is at full depth, in flow units)"
    ),
    
    links = c(
      "flow rate (flow units)",
      "average water depth (ft or m)",
      "flow velocity (ft/s or m/s)",
      "volume of water (ft3 or m3)",
      "capacity (fraction of full area filled by flow for conduits; control setting for pumps and regulators)"
    ),
    
    system = c(
      "air temperature (deg. F or deg. C)",
      "total rainfall (in/hr or mm/hr)",
      "total snow depth (inches or millimeters)",
      "average losses (in/hr or mm/hr)",
      "total runoff (flow units)",
      "total dry weather inflow (flow units)",
      "total groundwater inflow (flow units)",
      "total RDII inflow (flow units)",
      "total external inflow (flow units)",
      "total direct inflow (flow units)",
      "total external flooding (flow units)",
      "total outflow from outfalls (flow units)",
      "total nodal storage volume (ft3 or m3)",
      "potential evaporation (in/day or mm/day)",
      "actual evaporation (in/day or mm/day)"
    )
  )

  type_pollutants <- list(
    subcatchments = "washoff concentration of pollutant %s (mass/liter)",
    nodes = "concentration of pollutant %s after any treatment (mass/liter)",
    links = "concentration of pollutant %s (mass/liter)"
  )
  
  # 0: subcatchments, 1: nodes, 2: links, 3: system variables
  if (iType %in% 0:3) {
    
    element <- c("subcatchments", "nodes", "links", "system")[iType + 1]
    
    choices <- type_choices[[element]]

    # Add pollutants if any but not for system variables (iType == 3)
    if (iType != 3 && ! is.null(PollNames)) {
      choices <- c(choices, sprintf(type_pollutants[[element]] , PollNames))
    }
    
    vIndexStr <- if (is.null(vIndex)) {
      utils::select.list(choices = choices, multiple = TRUE)  
    } else {
      choices[vIndex + 1]
    }    

  } else {
    
    clean_stop("bad iType")
  }
  
  # create list with numeric vector with base 0 and var names
  result <- list(
    vIndex = match(vIndexStr, choices) - 1,
    names = gsub(" ", "_", gsub(" \\(.*$", "", vIndexStr))
  )

  # final check if selection is OK.
  if (identical(result$vIndex, numeric(0))) clean_stop("Unclear vIndex.")
  
  result
}
