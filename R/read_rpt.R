#' Read SWMM's .rpt file
#'
#' Reads a SWMM .rpt file and creates a list with corresponding results sections.
#' 
#' @param x Name (incl. path) to an report file.
#' @return An object of class `rpt`
#' @examples  
#' \dontrun{
#' list_of_rpt_results <- read_rpt("model.rpt")
#' } 
#' @rdname read_rpt
#' @export
read_rpt <- function(x) {
 
  # read lines and trimws
  rpt_lines <- readLines(x) %>% 
    trimws(.) %>% 
    .[!grepl("---------", .)]
  
  # results
  result_sections <- c("Element Count",
                       "Pollutant Summary",
                       "Landuse Summary",
                       "Raingage Summary",
                       "Subcatchment Summary",
                       "Node Summary",
                       "Link Summary",
                       "Cross Section Summary",
                       "Analysis Options",
                       "Runoff Quantity Continuity", 
                       "Runoff Quality Continuity",
                       "Flow Routing Continuity",
                       "Quality Routing Continuity",
                       "Highest Flow Instability Indexes", 
                       "Routing Time Step Summary", 
                       "Subcatchment Runoff Summary",
                       "Subcatchment Washoff Summary",
                       "Node Depth Summary", 
                       "Node Inflow Summary", 
                       "Node Flooding Summary", 
                       "Outfall Loading Summary", 
                       "Link Flow Summary", 
                       "Conduit Surcharge Summary",
                       "Link Pollutant Load Summary")

  # find section start
  section_start <- purrr::map(result_sections, ~ grep(., x = rpt_lines) - 1) %>% 
    purrr::map_if(., ~ identical(., integer(0)), ~ NA) %>% 
    unlist(.)
  
  # get end per section
  section_end <- c(section_start[-1] - 3, length(rpt_lines) - 5)
  
  # remove empty sections (and skip section name)
  section_not_emtpy <- (section_end-section_start > 0)
  section <- list(start = section_start[section_not_emtpy],
                  end = section_end[section_not_emtpy], 
                  name = result_sections[section_not_emtpy])
  
  # create list with sections  
  list_of_sections <- section %>% 
    purrr::transpose() %>% 
    purrr::map( ~ rpt_lines[.$start:.$end]) %>% 
    purrr::set_names(gsub("\\s+", "_", base::tolower(section$name)))
  
  # parse sections individually
  res <- purrr::imap(list_of_sections, ~ section_to_tbl(.x, .y)) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections were parsed but empty)
    purrr::discard( ~ nrow(.) < 1)
  
  # assign class attribute
  class(res) <- "rpt"
  
  return(res)
  
}