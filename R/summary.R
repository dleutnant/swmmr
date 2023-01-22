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
summary.inp <- function(object, ...)
{
  x <- object
  
  # from options take ...
  # infiltration_model
  # flow_units
  # flow_routing
  option_names <- c(
    "infiltration", 
    "flow_units", 
    "flow_routing", 
    "start_date", 
    "end_date"
  )
  
  swmm_options <- option_names %>% 
    purrr::map_chr(
      ~ x$options$Value[which(tolower(x$options$Option) == .x)]
    ) %>% 
    purrr::set_names(option_names) %>% 
    tolower()
  
  # Define sections which are of interest. Use those sections that have a number 
  # in column "summary" of "sections.csv"
  info <- section_info()
  info <- info[info$type == "input" & !is.na(info$summary), ]
  
  # Order by number in summary column first and by section name second
  section_names <- info$section[order(info$summary, info$section)]
  
  #todo: controls are not correctly interpreted
  
  # get length of unique elements per sections
  swmm_sections <- section_names %>% 
    #wrong: purrr::map_dbl( ~ nrow(x[[.]]) %||% 0) %>%
    purrr::map_int( ~ length(unique(x[[.]][[1]])) %||% 0) %>% 
    purrr::set_names(section_names)
  
  cat(
    "\n** summary of swmm model structure **", 
    paste0("\n", sprintf("%-14s : %10s", names(swmm_options), swmm_options)), 
    paste0("\n", sprintf("%-14s : %10s", names(swmm_sections), swmm_sections)), 
    "\n*************************************\n"
  )
  
  # return inp object invisible
  invisible(x)
}
