#' Read SWMM's .inp file
#'
#' Reads a SWMM .inp file and creates a list with corresponding SWMM sections.
#' 
#' @param x Name (incl. path) to an input file.
#' @param rm.comment Should lines with comments starting with a ";" be discarded?
#' @return An object of class `inp`
#' @examples  
#' \dontrun{
#' list_of_inp_sections <- read_inp("model.inp")
#' } 
#' @rdname read_inp
#' @export 
read_inp <- function(x, rm.comment = TRUE) {
  
  # read lines
  inp_lines <- readLines(x)
  
  # find section start
  section_start <- grep("\\[", inp_lines, value = F)
  
  # get section names
  section_names <- gsub(pattern = "[[:punct:]]", 
                        replacement = "",  
                        x = inp_lines[section_start])
  
  # get end per section
  section_end <- c(section_start[-1]-2, length(inp_lines))
  
  # remove empty sections (and skip section name)
  section_not_emtpy <- (section_end-section_start > 0)
  section <- list(start = section_start[section_not_emtpy] + 1,
                  end = section_end[section_not_emtpy], 
                  name = section_names[section_not_emtpy])
  
  # create list with sections  
  list_of_sections <- section %>% 
    purrr::transpose() %>% 
    purrr::map( ~ inp_lines[.$start:.$end]) %>% 
    purrr::set_names(base::tolower(section$name))
  
  # parse sections individually
  res <- purrr::imap(list_of_sections, ~ parse_inp(.x, .y, rm.comment = rm.comment)) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections are parsed but emtpy)
    purrr::discard( ~ nrow(.) < 1)

  # assign class attribute
  class(res) <- "inp"
  
  return(res)
  
}

#' import helper
#' @keywords internal
parse_inp <- function(x, section_name, rm.comment) {

  # remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # remove comments
  if (rm.comment) x <- x[!startsWith(x, ";")]
  
  # convert character vector to tibble
  x <- tibble::as_tibble(x)
  
  # add section as class to prepare generic parser
  class(x) <- c(class(x), section_name)

  # generic parser
  x <- parse_section(x)
  
  # if a section is not parsed, we return NULL
  if (is.null(x)) return (NULL)
  
  # remove dummy columns which names starts with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # trimws of character columns
  x <- dplyr::mutate_if(x, is.character, trimws)
  
  # section class got lost while formatting to tibble, so add it again
  class(x) <- c(class(x), section_name)

  # always return a tibble
  return(x)
}