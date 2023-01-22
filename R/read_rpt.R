#' Read SWMM's .rpt file
#'
#' Reads a SWMM .rpt file and creates a list with corresponding results sections.
#' 
#' @param x Name (incl. path) to an report file.
#' @return An object of class `rpt`
#' @param ... optional arguments passed to \code{\link[readr]{read_lines}}
#' @examples  
#' \dontrun{
#' list_of_rpt_results <- read_rpt("model.rpt")
#' } 
#' @rdname read_rpt
#' @export
read_rpt <- function(x, ...)
{
  # read lines and trimws
  report_lines <- readr::read_lines(x, ...) %>% 
    trimws(.) %>% 
    .[!grepl("---------", .)]
  
  # which sections are available?
  section_available <- purrr::map_lgl(
    report_sections, ~ any(grepl(., x = report_lines))
  )
  
  # last three lines contain analysis_info data
  # remove lines and add analysis_info to final list
  idx_last_lines <- tail(seq_along(report_lines), 3L)
  analysis_info <- tibble::tibble(value = report_lines[idx_last_lines])
  report_lines <- report_lines[-idx_last_lines]
  
  # if no sections can be found, we got errors
  if (!any(section_available)) {
    
    message("There are errors.")
    result <- section_to_tbl(x = report_lines, section_name = "rpt_error")
    result <- list(error = result, analysis_info = analysis_info)
    
    # Return result with class attribute set
    return(set_class(result, "rpt_error"))
  }
  
  # subset to available sections only
  report_sections <- report_sections[section_available] %>% 
    # sort vector
    purrr::map_int(., ~ grep(., x = report_lines)) %>% 
    purrr::set_names(report_sections[section_available]) %>% 
    sort %>% 
    names
  
  # find section start
  section_start <- report_sections %>%
    purrr::map(~ grep(., x = report_lines) - 1L) %>%
    purrr::map_if(
      ., 
      ~ identical(., integer(0L)) | identical(., numeric(0L)), 
      ~ NA
    ) %>% 
    as.integer(.)
  
  # get end per section
  section_end <- purrr::map(section_start[-1L] - 3L, function(x) {
    
    # we need this only for sections before analysis options
    if (startsWith(report_lines[x], "not just")) {
      x <- x - 7L
    }
    
    x
    
  }) %>%
    # remove last three lines (analysis)
    c(., length(report_lines) - 3L) %>% 
    as.integer()
  
  # occasionally, swmm produces less than 2 empty lines between sections
  for (i in seq_along(section_end)) {
    
    x <- section_end[i]
    
    # check if subsequent line is empty
    while (!identical(report_lines[x + 1L], "") & x < length(report_lines)) {
      x <- x + 1L
    }
    
    section_end[i] <- x
  }
  
  # remove empty sections (and skip section name)
  section_not_emtpy <- (section_end - section_start > 0L)
  
  section <- list(
    start = section_start[section_not_emtpy],
    end = section_end[section_not_emtpy], 
    name = report_sections[section_not_emtpy]
  )
  
  # create list with sections  
  list_of_sections <- section %>% 
    purrr::transpose() %>% 
    purrr::map( ~ report_lines[.$start:.$end]) %>% 
    purrr::set_names(gsub("\\s+|-", "_", base::tolower(section$name)))
  
  # parse sections individually
  result <- purrr::imap(list_of_sections, ~ section_to_tbl(.x, .y)) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections were parsed but empty)
    purrr::discard( ~ nrow(.) < 1L)
  
  # add analysis info
  result$analysis_info <- analysis_info
  
  # assign class attribute
  set_class(result, "rpt")
}
