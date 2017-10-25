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
  
  # find section lines
  section_lines <- grep("\\[", inp_lines, value = F)
  
  # get section names
  section_types <- gsub(pattern = "[[:punct:]]", 
                        replacement = "",  
                        x = inp_lines[section_lines])
  
  # get last line per section
  section_length <- c(section_lines[-1]-2,length(inp_lines)) 
  
  # create list with sections  
  list_of_sections <- purrr::map2(section_lines, section_length, 
                                  ~ inp_lines[(.x + 1):.y]) %>% 
    purrr::set_names(base::tolower(section_types))
  
  # parse sections individually
  res <- purrr::imap(list_of_sections, ~ parse_inp(.x, .y, rm.comment = rm.comment)) %>% 
    # discard nulls to keep list of sections as simple as possible
    purrr::discard(is.null)
  
  # assign class attribute
  class(res) <- "inp"
  
  return(res)
  
}

#' import helper
#' @keywords internal
parse_inp <- function(x, section_type, rm.comment) {
  
  # remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # remove comments
  if (rm.comment) x <- x[!startsWith(x, ";")]
  
  # convert character vector to tibble
  x <- tibble::as_tibble(x)
  
  # add section as class to prepare generic parser
  class(x) <- c(class(x), section_type)
  
  # generic parser
  x <- parse_section(x)
  
  # if a section is not parsed, we return NULL
  if (is.null(x)) return (NULL)
  
  # remove dummy columns which names starts with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # section class got lost while formatting to tibble, so add it again
  class(x) <- c(class(x), section_type)
  
  # always return a tibble
  return(x)
}