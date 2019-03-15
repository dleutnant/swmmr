#' Show summary of a swmm model structure
#'
#' @param object An object of class 'inp', created by \code{\link{read_inp}}.
#' @param ... Additional parameters (currently ignored).
#' @return The input is returned invisibly.
#' @examples
#' \dontrun{
#' x <- read_inp("model.inp")
#' summary(x)
#' } 
#' @rdname summary
#' @export
summary.inp <- function(object, ...) {
  
  x <- object
  
  # from options take ...
  # infiltration_model
  # flow_units
  # flow_routing
  vec_opts <- c("infiltration", "flow_units", "flow_routing", 
                "start_date", "end_date")
  
  opts <- vec_opts %>% 
    purrr::map_chr( ~ x$options$Value[which(tolower(x$options$Option) == .x)]) %>% 
    purrr::set_names(vec_opts) %>% 
    tolower()
  
  # define sections which are of interest
  vec_sects <- c("raingages", "subcatchments", "aquifers", "snowpacks",
                 "junctions", "outfalls", "dividers", "storages", "conduits",
                 "pumps", "orifices", "weirs", "outlets",
                 "controls", "pollutants", "landuses",
                 # Inflows
                 "lid_controls", "treatment")
  
  #todo: controls are not correctly interpreted
  
  # get length of unique elements per sections
  sects <- vec_sects %>% 
    #wrong: purrr::map_dbl( ~ nrow(x[[.]]) %||% 0) %>%
    purrr::map_int( ~ length(unique(x[[.]][[1]])) %||% 0) %>% 
    purrr::set_names(vec_sects)
  
  cat("\n** summary of swmm model structure **", 
      paste0("\n", sprintf("%-14s : %10s", names(opts), opts)), 
      paste0("\n", sprintf("%-14s : %10s", names(sects), sects)), 
      "\n*************************************\n")
  
  # return inp object invisible
  invisible(x)
  
}
