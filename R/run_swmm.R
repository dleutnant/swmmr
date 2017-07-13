#' @title Initiate a simulation run
#' @description This function runs a swmm inp file. If rpt and out files are not 
#' specified files are automatically created in the same directory of
#' the inp file.
#' @param inp Name and path to an input file.
#' @param rpt Name and path to a report file.
#' @param out Name and path to an out file.
#' @param exec Name and path to swmm5 executable. If not manually set, the following paths are looked up:
#' linux: "/usr/bin/swmm5" 
#' darwin: "/Applications/swmm5"
#' windows: "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe"
#' @inheritParams base::system2
#' @examples  
#' \dontrun{
#' result <- run_swmm("model.inp")
#' }
#' @rdname run_swmm
#' @export
run_swmm <- function(inp, 
                     rpt = NULL,
                     out = NULL,
                     exec = NULL,
                     stdout = "", 
                     wait = TRUE) {
  
  # check if inp and file exists
  stopifnot(file.exists(inp))
  
  # get the path of the executable on the precoded paths...
  if (is.null(exec)) exec <- .get_path_to_exec()
  
  # get the name of the directory which is used to create rpt and out files if not provided.
  dirn <- base::dirname(inp)

  # get the name of the inp file
  filename <- sub("^([^.]*).*", "\\1", base::basename(inp)) 
  
  # if rpt file is not provided, create one next to the inp file.
  if (is.null(rpt)) {
    rpt <-  file.path(dirn, paste(filename, "rpt", sep = "."))
  }
  
  # if out file is not provided, create one next to the inp file.
  if (is.null(out)) {
    out <-  file.path(dirn, paste(filename, "out", sep = "."))
  }
  
  # execute command
  base::system2(command = exec, 
                args = c(inp, rpt, out), 
                stdout = stdout,
                stderr = stdout,
                wait = wait,
                minimized = FALSE,
                invisible = TRUE)
  
  # flush console
  utils::flush.console()
  
  # return a list with file paths
  invisible(list(inp = normalizePath(inp),
                 rpt = normalizePath(rpt), 
                 out = normalizePath(out)))
  
}

#' Check if SWMM executable is in one of the expected directories.
#' @keywords internal
.get_path_to_exec <- function() {
  
  os <- .get_os()
  
  exec <- switch(os,
                 windows = "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe",
                 linux   = "/usr/bin/swmm5",
                 darwin  = "/Applications/swmm5")
  
  if (!file.exists(exec)) stop("Could not find swmm executable.") 
  
  return(exec)
  
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