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
  names(list_inp) <- section_title
  
  return(list_inp)
}
