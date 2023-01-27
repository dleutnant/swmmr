# create_temp_directories ------------------------------------------------------
create_temp_directories <- function(
    names = sprintf("dir_%d_", seq_len(n)), 
    n = 1L
)
{
  paths <- get_temp_paths(names, n)
  
  for (path in paths) {
    dir.create(path)
  }
  
  paths
}

# get_temp_paths ---------------------------------------------------------------
#' @importFrom purrr walk
get_temp_paths <- function(names = sprintf("file_%d_", seq_len(n)), n = 1L)
{
  if (length(names) > 0L) {
    sapply(names, tempfile)
  }
}

# example_input_files ----------------------------------------------------------
example_input_files <- function(ids = 1:6)
{
  system_file(
    "extdata", 
    paste0("Example", ids, ".inp"), 
    mustWork = TRUE
  )
}

# existing_path_or_null --------------------------------------------------------
existing_path_or_null <- function(...)
{
  path <- file.path(...)
  
  if (length(path[1L]) > 0L && file.exists(path[1L])) {
    path
  } # else NULL implicitly
}

# read_example_input_files -----------------------------------------------------
#' @importFrom purrr map
read_example_input_files <- function(ids = 1:6)
{
  # Get the paths to the inp files
  inp_files <- example_input_files(ids = ids)
  
  # Read the inp files and name the list elements according to the file names
  purrr::map(inp_files, read_inp) %>%
    stats::setNames(basename(inp_files))
}
