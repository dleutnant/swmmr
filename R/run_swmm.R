#' run_swmm
#'
#' Run a SWMM inp file.
#'
#' @title run_swmm
#' @param inp Name and path to an input file.
#' @param rpt Name and path to an report file.
#' @param out Name and path to an out file.
#' @param exe Name and path to swmm5.exe.
#' @param show.progress logical. Defines whether simulation progress is printed.
#' @rdname run_swmm
#' @export run_swmm
run_swmm <- function(inp, 
                     rpt = NULL,
                     out = NULL,
                     exe = "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe",
                     show.progress = T) {
  
  dirn <- dirname(inp)

  filename <- sub("^([^.]*).*", "\\1", basename(inp)) 
  
  if (!file.exists(exe)) stop("Could not find swmm5.exe.") 
  
  if (is.null(rpt)) {
    rpt <-  file.path(dirn, paste(filename, "rpt", sep = "."))
    
  }
  
  if (is.null(out)) {
    out <-  file.path(dirn, paste(filename, "out", sep = "."))
  }
  
  inp <- paste('"', inp,'"', sep = "")
  rpt <- paste('"', rpt,'"', sep = "")
  out <- paste('"', out,'"', sep = "")
  exe <- paste('"', exe,'"', sep = "")
  
  command <-  paste(exe, inp, rpt, out, sep = " ")
  
  if (show.progress) {
    print(paste("Executing: ", command, sep = "", collapse = ""))
    system(command, show.output.on.console = TRUE)
  }else{
    system(command, show.output.on.console = FALSE)
  }
  
  utils::flush.console()
}