# create_temp_directories ------------------------------------------------------
create_temp_directories <- function(n)
{
  paths <- create_temp_paths(n)
  purrr::walk(paths, ~ dir.create(path = .))
  paths
}

# create_temp_paths ------------------------------------------------------------
#' @importFrom purrr walk
create_temp_paths <- function(n)
{
  purrr::map(seq_len(n), ~ tempfile())
}

# example_input_files ----------------------------------------------------------
example_input_files <- function(ids = 1:6)
{
  system.file(
    "extdata", 
    paste0("Example", ids, ".inp"), 
    package = "swmmr", 
    mustWork = TRUE
  )
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

# read_example_input_files -----------------------------------------------------
#' @importFrom purrr map
read_example_input_files <- function(ids = 1:6)
{
  # Get the paths to the inp files
  inp_files <- example_input_files(ids = ids)
  
  # Read the inp files
  inputs <- purrr::map(inp_files, read_inp)
  
  # Name the list elements according to the file names
  stats::setNames(inputs, basename(inp_files))
}
