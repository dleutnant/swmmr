#' Get the type of SWMM element
#' @keywords internal
.get_iType <- function(iType=NULL) {
  
  choices <- c("subcatchment","node","link", "system variable")

  if (is.null(iType)) {
    iTypeStr <- utils::select.list(choices = choices, multiple = FALSE)  
  } else {
    if (length(iType) > 1) warning("only first iType is selected...")
    iTypeStr <- choices[iType[1] + 1] # to be consistent --> + 1
  }
  
  # create list with numeric vector with base 0 and var names
  result <- list(iType = which(choices %in% iTypeStr) - 1,
                 names = iTypeStr)
  
  if (identical(iType, numeric(0))) stop("Unclear iType.",call. = FALSE)
  
  return(result)
  
}

#' Get the names of SWMM elements
#' @keywords internal
.get_iIndex <- function(list_of_elements, iType=NULL, object_name=NULL) {

  # subcatchments
  if (iType == 0) {
    
    if (length(list_of_elements$subcatchments$names) > 0) {
      
      if (is.null(object_name)) {
        object_name <- utils::select.list(list_of_elements$subcatchments$names, 
                                   multiple = TRUE)
      }
      
      result <- list(iIndex = match(object_name, list_of_elements$subcatchments$names) - 1,
                     names = object_name)
      
    } else {
      warning("no subcatchments ")
    }  
    
  } else {
    
    # nodes
    if (iType == 1) {
      
      if (length(list_of_elements$nodes$names) > 0) {
        
        if (is.null(object_name)) {
          object_name <- utils::select.list(list_of_elements$nodes$names, 
                                            multiple = TRUE)
        }
        
        result <- list(iIndex = match(object_name, list_of_elements$nodes$names) - 1,
                       names = object_name)
        
      } else {
        warning("no nodes")
      }
      
    } else {

      # links
      if (iType == 2) {
        if (length(list_of_elements$links$names) > 0) {
          if (is.null(object_name)) {
            object_name <- utils::select.list(list_of_elements$links$names, 
                                              multiple = TRUE)
          } 
          
          result <- list(iIndex = match(object_name, list_of_elements$links$names) - 1,
                         names = object_name)
          
        } else {
          warning("no links")
        }
      } else {
        
        warning("Unclear iType.")
        
      }
    }
  }
  if (anyNA(result$iIndex)) stop("Unclear iIndex.", call. = FALSE)
  if (identical(result$iIndex, numeric(0))) stop("Unclear iIndex.", call. = FALSE)
  
  return(result)
}

#' Get the simulated values
#' @keywords internal
.get_vIndex <- function(iType, vIndex=NULL, PollNames = NULL) {
  
  # subcatchments
  if (iType == 0) {
    
    choices <- c("rainfall rate (in/hr or mm/hr)",
                 "snow depth (inches or millimeters)",
                 "evaporation loss (in/day or mm/day)",
                 "infiltration loss (in/hr or mm/hr)",
                 "runoff flow (flow units)",
                 "groundwater flow into the drainage network (flow units)",
                 "groundwater elevation (ft or m)",
                 "soil moisture in the unsaturated groundwater zone (volume fraction)")
    
    if (!is.null(PollNames)) choices <- c(choices, paste("washoff concentration of pollutant", 
                                                         PollNames, 
                                                         "(mass/liter)"))
    
    if (is.null(vIndex)) {
      vIndexStr <- utils::select.list(choices = choices, multiple = TRUE)  
    } else {
      vIndexStr <- choices[vIndex + 1]
    }
      
    
  } else {
    
    # nodes
    if (iType == 1) {
      
      choices <- c("water depth (ft or m above the node invert elevation)",
                   "hydraulic head (ft or m, absolute elevation per vertical datum)",
                   "stored water volume (including ponded water, ft3 or m3)",
                   "lateral inflow (runoff + all other external inflows, in flow units)",
                   "total inflow (lateral inflow + upstream inflows, in flow units)",
                   "surface flooding (excess overflow when the node is at full depth, in flow units)")
      
      if (!is.null(PollNames)) choices <- c(choices, paste("concentration of pollutant", 
                                                           PollNames, 
                                                           "after any treatment (mass/liter)"))
      
      if (is.null(vIndex)) {
        vIndexStr <- utils::select.list(choices = choices, multiple = TRUE)  
      } else {
        vIndexStr <- choices[vIndex + 1]
      }
      
    } else { 
    
      # links
      if (iType == 2) {
        
        choices <- c("flow rate (flow units)",
                     "average water depth (ft or m)",
                     "flow velocity (ft/s or m/s)",
                     "volume of water (ft3 or m3)",
                     "capacity (fraction of full area filled by flow for conduits; control setting for pumps and regulators)")
        
        if (!is.null(PollNames)) choices <- c(choices, paste("concentration of pollutant", 
                                                             PollNames, 
                                                             "(mass/liter)"))
        
        if (is.null(vIndex)) {
          vIndexStr <- utils::select.list(choices = choices, multiple = TRUE)  
        } else {
          vIndexStr <- choices[vIndex + 1]
        }
        
      } else {
        
        # system variables
        if (iType == 3) {
          
          choices <- c("air temperature (deg. F or deg. C)",
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
                       "actual evaporation (in/day or mm/day)")
          
          if (is.null(vIndex)) {
            vIndexStr <- utils::select.list(choices = choices, multiple = TRUE)  
          } else {
            vIndexStr <- choices[vIndex + 1]
          }

        } else {
          
          stop("bad iType", call. = FALSE)
          
        }
        
      }
    
    }
  }
  
  # create list with numeric vector with base 0 and var names
  result <- list(vIndex = which(choices %in% vIndexStr) - 1,
                 names = sapply(strsplit(substr(vIndexStr,
                                                1,
                                                as.numeric(gregexpr(" \\(",
                                                                    vIndexStr)) - 1),
                                         split = " "),
                                paste0, 
                                collapse = "_"))

  # final check if selection is OK.
  if (identical(result$vIndex, numeric(0))) stop("Unclear vIndex.",call. = FALSE)
  
  return(result)
  
}