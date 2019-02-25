#' @title Initiate a simulation run
#' @description This function runs a swmm inp file. If rpt and out files are not 
#' specified files are automatically created in the same directory of
#' the inp file.
#' @param inp Name and path to an input file.
#' @param rpt Name and path to a report file.
#' @param out Name and path to an out file.
#' @param exec Name and path to swmm5 executable. If not manually set, 
#' the following paths are looked up when package gets loaded:
#' windows: "C:/Program Files (x86)/EPA SWMM 5.X.XXX/swmm5.exe" 
#' not windows: "/usr/local/bin/swmm5" , "/usr/bin/swmm5"
#' @details The path to a swmm5 executable is read by calling 'getOption("swmmr.exec")'.
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
  
  # get the path of the executable on the precoded paths...
  if (is.null(exec)) exec <- getOption("swmmr.exec")
  
  # check if inp and exec exists
  stopifnot(file.exists(inp), file.exists(exec))
  
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
