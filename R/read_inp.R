#' Read SWMM's .inp file
#'
#' Reads a SWMM .inp file and creates a list with corresponding SWMM sections.
#' This function reads the .inp in a very simplified but reasonable manner.
#' It's main purpose is to glance the model structure and simulation options set. 
#' 
#' @param inp Name (incl. path) to an input file.
#' @param rm.comment Should lines with comments starting with a ";" be discarded?
#' @return A list with SWMM inp sections.
#' @examples  
#' \dontrun{
#' inp_sections <- read_inp("model.inp")
#' } 
#' @rdname read_inp
#' @export 
read_inp <- function(inp, rm.comment = TRUE) {
  
  # Import raw data:
  inp_raw <- base::readLines(inp)
  
  # to support simplified parsing strategy "separating columns by white spaces",
  # these words need to be filled...
  words_with_ws <- c("Project Title/Notes",
                     "Data Source",
                     "Rain Gage",
                     "Stage Data",
                     "Curve Name/Params",
                     "From Node",
                     "To Node",
                     "Pump Curve")
  
  # ... so lets change the words in the raw text vector...
  for (i in seq_along(words_with_ws)) inp_raw <- gsub(pattern = words_with_ws[i], 
                                                      replacement = gsub(" ", "_", words_with_ws[i]),
                                                      x = inp_raw, 
                                                      fixed = TRUE)
  
  # remove divider lines
  inp_raw <- inp_raw[!grepl("-----", inp_raw)]
  
  # find section lines
  section_lines <- grep("\\[", inp_raw, value = F)
  
  section_title <- gsub(pattern = "\\[|\\]", 
                        replacement = "",
                        x = inp_raw[section_lines])
  
  header_lines <- data_blocks <- list_inp <- vector(length = length(section_lines), mode = "list")
  
  # get header lines
  for (i in seq_len(length(header_lines))) {
    
    line <- inp_raw[section_lines[i] + 1]
    
    # section has columns
    if (grepl(";;", substr(line, 1, 2), fixed = TRUE)) {
      
      line <- gsub(";;", "", line)
      line <- gsub("\\s+", "_-_", line)
      
      header_lines[[i]] <- unlist(strsplit(line, "_-_"))
      data_blocks[[i]]$start <- section_lines[i] + 2
      
    # sectios has no columns  
    } else {
      
      header_lines[[i]] <- NA
      data_blocks[[i]]$start <- section_lines[i] + 1
      
    }
    
    data_blocks[[i]]$end <- max(min(section_lines[i + 1] - 2, length(inp_raw), na.rm = TRUE),
                                data_blocks[[i]]$start)
                                
    
  }
  
  # create list of chunks 
  inp_ranges <- sapply(data_blocks, function(x) seq(from = x$start, to = x$end))
  
  # read chunks
  list_inp <- lapply(inp_ranges, function(x) {
    
    text <- inp_raw[x]
    
    # remove comments
    if (rm.comment) text <- text[!grepl(";", text)]
    
    if (identical(text, "")) {
      return(data.frame(NA, NA, NA, NA, NA, NA))
    } else {
      
      utils::read.table(text = text,
                        sep = "", 
                        dec = ".", 
                        numerals = "no.loss",
                        stringsAsFactors = FALSE, 
                        fill = TRUE)
    }
  })

  # because we set previously header = FALSE to circumvent non harmonized headers, 
  # we need to set the column names manually
  for (i in seq_len(length(header_lines))) {
    # get length of colnames
    dummy <- colnames(list_inp[[i]])
    colnames(list_inp[[i]]) <- c(header_lines[[i]], dummy)[seq_len(length(dummy))]
  }
  
  # rename chunks
  names(list_inp) <- tolower(section_title)
  
  # assign class "inp"
  class(list_inp) <- "inp"
  
  # change to tibble
  #list_inp <- lapply(list_inp, tibble::as_tibble)
    
  return(list_inp)
}

#' Read SWMM's .inp file
#'
#' Reads a SWMM .inp file and creates a list with corresponding SWMM sections.
#' 
#' @param inp Name (incl. path) to an input file.
#' @param rm.comment Should lines with comments starting with a ";" be discarded?
#' @return An object of class `inp`
#' @examples  
#' \dontrun{
#' inp_sections <- read_inp("model.inp")
#' } 
#' @rdname read_inp2
#' @export 
read_inp2 <- function(x, rm.comment = TRUE) {
  
  #x <- "../../GitLab/models/residential/qm_rc_tmpl.inp"
  
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
  res <- purrr::imap(list_of_sections, ~ parse_section(.x, .y, rm.comment = rm.comment)) %>% 
    # discard nulls to keep list as simple as possible
    purrr::discard(is.null)
  
  # assign class attribute
  class(res) <- "inp"
  
  return(res)
  
}

#' import helper
#' @keywords internal
parse_section <- function(x, section_type, rm.comment) {
  
  # remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # remove comments
  if (rm.comment) x <- x[!startsWith(x, ";")]
  
  # convert character vector to tibble
  x <- tibble::as_tibble(x)
  
  # switch between section_types
  x <- switch (section_type,
               "options" = import_options(x),
               "title" = import_title(x),
               "raingages" = import_raingages(x),
               "subcatchments" = import_subcatchments(x),
               "subareas" = import_subareas(x),
               "infiltration" = import_infiltration(x),
               "junctions" = import_junctions(x),
               "conduits" = import_conduits(x),
               "pollutants" = import_pollutants(x),
               "landuses" = import_landuses(x),
               "buildup" = import_buildup(x),
               "washoff" = import_washoff(x),
               "coordinates" = import_coordinates(x),
               "vertices" = import_vertices(x),
               "polygons" = import_polygons(x),
               "symbols" = import_symbols(x),
               "labels" = import_labels(x)
  )
  
  # if a section is not parsed, we yield NULL
  if (is.null(x)) return (NULL)
  
  # remove dummy columns which names starts with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # always return a tibble
  return(x)
}