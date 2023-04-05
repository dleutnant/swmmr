#' import helper
#' @keywords internal
section_to_tbl <- function(x, section_name, rm.comment = FALSE, options = NULL)
{
  # Remove header lines 
  x <- x[!startsWith(x, ";;")]
  
  # Remove comments
  if (rm.comment) {
    x <- x[!startsWith(x, ";")]
  }
  
  # Convert character vector to tibble with class set to section_name
  x <- convert_to_section(x, section_name)
  
  # Call the generic parser
  x <- if (section_name == "infiltration") {
    parse_section(x, inf_model = tolower(options$INFILTRATION))
  } else {
    parse_section(x)
  }
  
  # Return NULL if the section was not parsed
  if (is.null(x)) {
    return(NULL)
  }
  
  # Remove dummy columns of which names start with *tab 
  x <- x[, !grepl("^tab", colnames(x))]
  
  # Remove rows with NA's only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  
  # Define names of ID columns (that are assumed to be of type character)
  chr_cols <- intersect(names(x), c(
    "Name", "Link", "Links", "Subcatchment", "Outlet", "Node", "From Node", 
    "To Node", "Gage", "Pump"
  ))
  
  # Make sure ID columns are of type character and trim whitespace characters
  x[chr_cols] <- lapply(x[chr_cols], function(x) trimws(as.character(x)))
  
  x
}

# convert_to_section -----------------------------------------------------------
convert_to_section <- function(x, section_name)
{
  # Remove empty lines, convert to one-column tibble and add section as class 
  # to prepare generic parser
  data.frame(value = x[x != ""]) %>%
    tibble::as_tibble() %>%
    add_class(section_name)
}
