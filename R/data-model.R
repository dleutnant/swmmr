#
# Functions providing information related to the data model
#

#' Get List of Default Values for Columns for Different Objects
#' @keywords internal
get_column_defaults <- function()
{
  # Read default data from csv file
  defaults_data <- read_data_model("defaults.csv")
  
  # Vector of section names, as a factor
  f <- factor(defaults_data$section, levels = unique(defaults_data$section))
  
  # Split data into subsets each of which refers to a section
  defaults_list <- split(defaults_data, f = f)
  
  # Convert default values to given types
  lapply(defaults_list, function(defaults) {
    stats::setNames(nm = defaults$column, lapply(
      X = seq_len(nrow(defaults)), 
      FUN = function(i) convert_to_type(defaults$value[i], defaults$type[i])
    ))
  })
}

# get_column_dictionary --------------------------------------------------------

#' @importFrom tibble as_tibble
#' @importFrom utils read.csv
get_column_dictionary <- function(section = NULL)
{
  dictionary <- read_data_model("dictionary.csv") %>%
    tibble::as_tibble()
  
  if (is.null(section)) {
    return(dictionary)
  }
  
  dictionary[dictionary$section == section, , drop = FALSE]
}

# get_column_names -------------------------------------------------------------
get_column_names <- function(section, domain)
{
  dictionary <- get_column_dictionary(section)
  
  dictionary[[match.arg(domain, names(dictionary))]]
}

# get_section_names ------------------------------------------------------------
get_section_names <- function(type)
{
  info <- section_info()
  
  info$section[info$type == type]
}

# get_section_names_for_input --------------------------------------------------
get_section_names_for_input <- function()
{
  # Read section names from "sections.csv"  
  info <- section_info()
  info <- info[!is.na(info$input), ]
  
  # Order section names by number in column "input" first
  # and by the section name second
  info$section[order(info$input, info$section)]
}

# read_data_model --------------------------------------------------------------
read_data_model <- function(file_name)
{
  file <- system_file("extdata/config", file_name)
  
  structure(read.csv(file), file = file)
}

# section_columns --------------------------------------------------------------
section_columns <- function(section_name = NULL)
{
  columns_per_section <- read_data_model("columns.csv") %>%
    split(factor(.$section, unique(.$section))) %>%
    lapply("[[", "column")
  
  if (is.null(section_name)) {
    return(columns_per_section)
  }
  
  if (!section_name %in% names(columns_per_section)) {
    stop_formatted(
      "No columns defined for section '%s' in section_columns()", 
      section_name
    )
  }
  
  columns_per_section[[section_name]]
}

# section_info -----------------------------------------------------------------
section_info <- function(key = NULL)
{
  info <- read_data_model("sections.csv")
  
  if (is.null(key)) {
    return(info)
  }
  
  index <- which(info$key == key)
  
  if (length(index) != 1L) {
    stop_formatted(
      "No such key '%s' or key is not unique in '%s'.", 
      key, attr(info, "file")
    )
  }
  
  info[index, ]
}
