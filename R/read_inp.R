#' Read the SWMM .inp file
#'
#' Reads an SWMM .inp file and creates a list with corresponding SWMM-sections.
#' This function reads the .inp in a very simplified but reasonable manner.
#' It's main purpose is to glance the model structure and options set. 
#' 
#' @title read_inp
#' @param inp Name and path to an input file.
#' @rdname read_inp
#' @export read_inp
read_inp <- function(inp) {
  
  warning("This function reads the .inp in a very simplified manner! 
          It's main purpose is to glance the model structure and options set.")

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
  
  # substitute comment char
  inp_raw <- gsub(";;", "", inp_raw)
  
  # remove divider lines
  inp_divider_removed <- inp_raw[!grepl("-----", inp_raw)]
  
  # find section lines
  section_lines <- grep("\\[", inp_divider_removed, value = F)
  
  # make it more readable
  section_name <- gsub(pattern = "\\[|\\]", 
                       replacement = "",
                       x = inp_divider_removed[section_lines])
  
  # prepare list to store sections
  list_inp <- as.vector(1:length(section_name), mode = "list")
  
  # create list of chunks 
  inp_ranges <- sapply(1:length(section_lines), function(x) seq(from = section_lines[x] + 1,
                                                                to = min(section_lines[x + 1] - 2,
                                                                         length(inp_raw),
                                                                         na.rm = TRUE)))
  # read chunks
  list_inp <- lapply(inp_ranges, function(x) utils::read.csv(text = inp_divider_removed[x], 
                                                             sep = "",
                                                             header = T,
                                                             stringsAsFactors = F))
  # rename chunks
  names(list_inp) <- section_name
  
  return(list_inp)
  
  
}
