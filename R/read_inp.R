# input section
#' @keywords internal
input_sections <- c("aquifers","backdrop","buildup",
                    "conduits", "controls", "coordinates","coverages","curves",
                    "dividers","dwf",
                    "evaporation","events",
                    "files","groundwater","hydrographs",
                    "iiflows","infiltration","inflows",
                    "junctions",
                    "labels","landuses","lid_controls","lid_usage","loadings","losses",
                    "map",
                    "options", "orifices","outfalls","outlets",
                    "patterns","pollutants","polygons","profiles","pumps",
                    "raingages","report",
                    "snowpacks","storage","subareas","subcatchments","symbols",
                    "tags","temperature","timeseries","title","treatment",
                    "vertices",
                    "washoff","weirs",
                    "xsections")

#' Read SWMM's .inp file
#'
#' Reads a SWMM .inp file and creates a list with corresponding SWMM sections.
#' 
#' @param x Name (incl. path) to an input file.
#' @param rm.comment Should lines with comments starting with a ";" be discarded?
#' @param ... optional arguments passed to \code{\link[readr]{read_lines}}.
#' @return An object of class `inp`
#' @examples  
#' \dontrun{
#' list_of_inp_sections <- read_inp("model.inp")
#' } 
#' @rdname read_inp
#' @export 
read_inp <- function(x, rm.comment = TRUE, ...) {
  
  # read lines
  inp_lines <- readr::read_lines(x, ...)
  
  # delete leading whitespaces in strings
  inp_lines <- gsub("^\\s+", "", inp_lines)
  
  # find section start
  section_start <- grep("\\[", inp_lines, value = F)

  # get section names
  section_names <- gsub(pattern = "\\[|\\]",
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
  
  # get options
  if (is.null(list_of_sections$options)) {
    warning("inp file does not contain section 'options'")
    options <- NULL
  } else {
    opt <- section_to_tbl(x = list_of_sections$options, 
                          section_name = "options", 
                          rm.comment = TRUE)
    options <- as.list(opt$Value)
    names(options) <- opt$Option
  }
  
  # parse sections individually
  res <- purrr::imap(list_of_sections, ~ section_to_tbl(.x, .y, 
                                                        rm.comment = rm.comment, 
                                                        options = options)) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections were parsed but empty)
    purrr::discard( ~ nrow(.) < 1)
  
  # assign class attribute
  class(res) <- "inp"
  
  return(res)
  
}

