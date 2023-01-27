#' Get List of Default Values for Columns for Different Objects
#' @keywords internal
get_column_defaults <- function()
{
  # Read default data from csv file
  defaults_data <- read.csv(system_file("extdata/defaults.csv"))

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
