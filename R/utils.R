clean_stop <- function(...) {
  
  stop(..., call. = FALSE)
}

clean_warning <- function(...) {
  
  warning(..., call. = FALSE)
}

stop_on_bad_index <- function(index, choices) {

  # Possible values for index
  values <- seq_along(choices) - 1
  
  if (!is.numeric(index) || length(index) != 1L || !index %in% values) {
    clean_stop(sprintf(
      "The index must be an integer value between 0 and %d:\n%s", 
      values[length(values)], 
      paste(sprintf("%4i: %s", values, choices), collapse = "\n")
    ))
  }
}

#' Determine the OS
#' @keywords internal
.get_os <- function(){
  
  sysinf <- base::Sys.info()
  
  if (!is.null(sysinf)) {
    
    os <- sysinf['sysname']
    
  } else {
    
    os <- base::.Platform$OS.type
    
  }
  
  return(tolower(os))
}

#' Determine the latest swmm5.exe in the specified  program files folder
#' @keywords internal
.get_exec_on_windows <- function(path, prefix, exe) {
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
  
  return(full_name)
  
}

#' Determine the latest swmm5.exe in the specified  program files folder
#' @keywords internal
.get_exec_on_linux_or_mac <- function(path, exe) {
  
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
  return(full_name)
  
}