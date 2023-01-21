# input section
#' @keywords internal
input_sections <- c(
  "aquifers",
  "backdrop",
  "buildup",
  "conduits", 
  "controls", 
  "coordinates",
  "coverages",
  "curves",
  "dividers",
  "dwf",
  "evaporation",
  "events",
  "files",
  "groundwater",
  "hydrographs",
  "iiflows",
  "infiltration",
  "inflows",
  "junctions",
  "labels",
  "landuses",
  "lid_controls",
  "lid_usage",
  "loadings",
  "losses",
  "map",
  "options", 
  "orifices",
  "outfalls",
  "outlets",
  "patterns",
  "pollutants",
  "polygons",
  "profiles",
  "pumps",
  "raingages",
  "report",
  "snowpacks",
  "storage",
  "subareas",
  "subcatchments",
  "symbols",
  "tags",
  "temperature",
  "timeseries",
  "title",
  "treatment",
  "vertices",
  "washoff",
  "weirs",
  "xsections"
)

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
read_inp <- function(x, rm.comment = TRUE, ...)
{
  # read lines
  inp_lines <- readr::read_lines(x, ...)
  
  # delete leading whitespaces in strings
  inp_lines <- gsub("^\\s+", "", inp_lines)

  # get information on the sections in the file (line ranges)  
  section <- get_section_info(inp_lines)
  
  # create list with sections  
  list_of_sections <- section %>% 
    purrr::transpose() %>% 
    purrr::map( ~ inp_lines[.$start:.$end]) %>% 
    purrr::set_names(base::tolower(section$name))
  
  # get options
  options <- if (is.null(list_of_sections$options)) {
    clean_warning("inp file does not contain section 'options'")
  } else {
    opt <- section_to_tbl(
      x = list_of_sections$options, 
      section_name = "options", 
      rm.comment = TRUE
    )
    stats::setNames(as.list(opt$Value), opt$Option)
  }
  
  # parse sections individually
  res <- purrr::imap(
    list_of_sections, 
    ~ section_to_tbl(.x, .y, rm.comment = rm.comment, options = options)
  ) %>% 
    # discard nulls (nulls are returned if section is not parsed)
    purrr::discard(is.null) %>% 
    # discard empty tibbles (sections were parsed but empty)
    purrr::discard( ~ nrow(.) < 1)
  
  # assign class attribute
  class(res) <- "inp"
  
  res
}

# get_section_info -------------------------------------------------------------
get_section_info <- function(x)
{
  # find section start
  starts <- grep("\\[", x)
  
  # get section names
  sections <- gsub("\\[|\\]", "", x[starts])
  
  # get end per section
  ends <- c(starts[-1L] - 2L, length(x))
  
  # remove empty sections (and skip section name)
  is_not_empty <- (ends > starts)
  
  list(
    start = starts[is_not_empty] + 1L,
    end = ends[is_not_empty], 
    name = sections[is_not_empty]
  )
}
