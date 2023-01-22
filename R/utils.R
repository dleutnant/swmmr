# add_class --------------------------------------------------------------------
add_class <- function(x, cls)
{
  set_class(x, c(cls, class(x)))
}

# add_columns_if_missing -------------------------------------------------------

#' Add Columns With Default Values if Not in Data Frame
#' @keywords internal
add_columns_if_missing <- function(df, defaults, force = FALSE)
{
  for (column in names(defaults)) {
    df <- add_column_if_missing(df, column, defaults[[column]], force)
  }
  
  df
}

# add_column_if_missing --------------------------------------------------------

#' Add Column With Default Value if Not in Data Frame
#' @keywords internal
add_column_if_missing <- function(df, column, default, force = FALSE)
{
  if (force || ! column %in% colnames(df)) {
    df[[column]] <- default
  }
  
  df
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# clean_warning ----------------------------------------------------------------
clean_warning <- function(...)
{
  warning(..., call. = FALSE)
  invisible(NULL)
}

# comma_space_collapsed --------------------------------------------------------
comma_space_collapsed <- function(x)
{
  paste(x, collapse = ", ")
}

# convert_to_type --------------------------------------------------------------
convert_to_type <- function(x, type)
{
  do.call(paste0("as.", type), list(x))
}

# create_dir_if_required -------------------------------------------------------
create_dir_if_required <- function(path, silent = TRUE)
{
  if (!file.exists(path)) {
    if (!silent) {
      cat("Creating directory", path, "\n")
    }
    dir.create(path)
  }
}

# get_column_dictionary --------------------------------------------------------

#' @importFrom tibble as_tibble
#' @importFrom utils read.csv
get_column_dictionary <- function()
{
  file <- system_file("extdata/dictionary.csv")
  tibble::as_tibble(read.csv(file))
}

# get_section_names ------------------------------------------------------------
get_section_names <- function(type)
{
  info <- section_info()
  
  info$section[info$type == type]
}

# given ------------------------------------------------------------------------
given <- function(x)
{
  !is.null(x)
}

# in_brackets ------------------------------------------------------------------
in_brackets <- function(x)
{
  paste0("[", x, "]")
}

# is_empty ---------------------------------------------------------------------
is_empty <- function(x)
{
  is.na(x) | x == "" | grepl("^\\s+$", x)
}

# remove_at_indices ------------------------------------------------------------
remove_at_indices <- function(x, indices)
{
  if (length(indices) > 0L) x[-indices] else x
}

# replace_values ---------------------------------------------------------------
replace_values <- function(x, from, to)
{
  indices <- match(x, from)
  is_match <- !is.na(indices)
  x[is_match] <- to[indices[is_match]]
  x
}

# section_info -----------------------------------------------------------------
section_info <- function(key = NULL)
{
  file <- system_file("extdata/sections.csv")
  
  info <- read.csv(file)
  #info  <- read.csv(file = "inst/extdata/sections.csv")
  
  if (is.null(key)) {
    return(info)
  }
  
  index <- which(info$key == key)
  
  if (length(index) != 1L) {
    stop_formatted("No such key '%s' or key is not unique in '%s'.", key, file)
  }
  
  info[index, ]
}

# set_class --------------------------------------------------------------------
set_class <- function(x, cls)
{
  class(x) <- cls
  x
}

# skip_head --------------------------------------------------------------------

#' helper function skippting the first n rows of a data frame
#' @keywords internal
skip_head <- function(df, n)
{
  df[-seq_len(n), ]
}

# stop_on_bad_index ------------------------------------------------------------
stop_on_bad_index <- function(index, choices)
{
  # Possible values for index
  values <- seq_along(choices) - 1
  
  if (!is.numeric(index) || length(index) != 1L || !index %in% values) {
    stop_formatted(
      "The index must be an integer value between 0 and %d:\n%s", 
      values[length(values)], 
      paste(sprintf("%4i: %s", values, choices), collapse = "\n")
    )
  }
}

# stop_formatted ---------------------------------------------------------------
stop_formatted <- function(fmt, ...)
{
  clean_stop(sprintf(fmt, ...))
}

# stop_if_out_of_range ---------------------------------------------------------
stop_if_out_of_range <- function(x, a, b)
{
  if (x < a || x > b) {
    stop_formatted(
      "%s must be a value between %d and %d!",
      deparse(substitute(x)), a, b
    )
  }
}

# system_file ------------------------------------------------------------------
system_file <- function(...)
{
  system.file(..., package = "swmmr")
}

# trim_vector ------------------------------------------------------------------
trim_vector <- function(x, trim = "both")
{
  trim <- match.arg(trim, c("head", "tail", "both", "none"))
 
  if (trim %in% c("head", "both")) {
    x <- remove_at_indices(x, which_first(is_empty(x)))
  }
  
  if (trim %in% c("tail", "both")) {
    x <- remove_at_indices(x, which_last(is_empty(x)))
  }
  
  x
}

# unload_dll -------------------------------------------------------------------
unload_dll <- function()
{
  dyn.unload(system_file("libs/x64/swmmr.dll"))
}

# warn_formatted ---------------------------------------------------------------
warn_formatted <- function(fmt, ...)
{
  clean_warning(sprintf(fmt, ...))
}

#' Determine the OS
#' @keywords internal
.get_os <- function()
{
  sysinf <- base::Sys.info()
  
  os <- if (!is.null(sysinf)) {
    sysinf['sysname']
  } else {
    base::.Platform$OS.type
  }
  
  tolower(os)
}

#' Determine the path to the latest swmm5 executable
.get_exec <- function()
{
  if (.get_os() == "windows") {
    .get_exec_on_windows(
      path = "c:/Program Files (x86)/", 
      prefix = "EPA SWMM", 
      exe = "swmm5.exe"
    )
  } else {
    .get_exec_on_linux_or_mac(
      path = c("/usr/local/bin", "/usr/bin"), 
      exe = "swmm5"
    )
  }
}

#' Determine the latest swmm5.exe in the specified  program files folder
#' @keywords internal
.get_exec_on_windows <- function(path, prefix, exe)
{
  # path = "c:/Program Files (x86)/", 
  # prefix = "EPA SWMM",
  # exe = "swmm5.exe"
  
  # program directory
  version_vec <- list.dirs(path, recursive = FALSE) %>% 
    # get directories with EPA SWMM sequence
    grep(prefix, ., value = TRUE) %>% 
    # get the name of the directory only
    basename() %>% 
    # focus the version number
    gsub(prefix, "", .) %>% 
    # remove white spaces
    trimws()
  
  # modify compareVersion to be applicable with Reduce
  compareVersion_mod <- function(a, b) {
    winner <- utils::compareVersion(a, b)
    if (winner == -1) return(b)
    if (winner > -1) return(a)
    return(NA)
  }
  
  # find winner in vector
  version <- Reduce(compareVersion_mod, x = version_vec)
  
  # create full name to swmm5.exe
  full_name <- file.path(path, paste(prefix, version), exe)
  
  # final check
  if (!file.exists(full_name)) {
    message("SWMM executable not found.")
    full_name <- NULL
  }
  
  full_name
}

#' Determine the latest swmm5.exe in the specified  program files folder
#' @keywords internal
.get_exec_on_linux_or_mac <- function(path, exe)
{
  # create full name to swmm5 executable
  full_name <- file.path(path, exe)
  
  # check if file exists
  exec_found <- file.exists(full_name)
  
  # in case at least one file is found
  if (any(exec_found)) {
    full_name <- full_name[exec_found][1]
  } else {
    # otherwise raise warning
    message("SWMM executable not found.")
    full_name <- NULL
  }
  
  full_name
}

# which_first ------------------------------------------------------------------
which_first <- function(x)
{
  which_marginal(x, first = TRUE)
}

# which_last -------------------------------------------------------------------
which_last <- function(x)
{
  which_marginal(x, first = FALSE)
}

# which_marginal ---------------------------------------------------------------
which_marginal <- function(x, first)
{
  stopifnot(is.logical(x))
  stopifnot(identical(first, TRUE) || identical(first, FALSE))
  
  i <- which(x)
  
  start <- ifelse(first, 1L, length(x) - length(i) + 1L)
  
  i[i == seq.int(start, along.with = i)]
}
