# example_input_files ----------------------------------------------------------
example_input_files <- function()
{
  system.file(
    "extdata", 
    paste0("Example", 1:6, ".inp"), 
    package = "swmmr", 
    mustWork = TRUE
  )
}

# read_example_input_files -----------------------------------------------------
#' @importFrom purrr map
read_example_input_files <- function()
{
  # Get the paths to the inp files
  inp_files <- example_input_files()
  
  # Read the inp files
  inputs <- purrr::map(inp_files, read_inp)
  
  # Name the list elements according to the file names
  stats::setNames(inputs, basename(inp_files))
}

# existing_path_or_null --------------------------------------------------------
existing_path_or_null <- function(x)
{
  if (length(x[1]) > 0L && file.exists(x[1L])) {
    x
  } else {
    NULL
  }
}
