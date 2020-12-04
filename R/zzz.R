# inspired by http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname) {
  # load options available
  op <- options()
  
  # determine os
  os <- .get_os()
  
  # get path to executable
  swmmr.exec <- if (os == "windows") {
    # case os is windows
    .get_exec_on_windows(
      path = "c:/Program Files (x86)/", prefix = "EPA SWMM", exe = "swmm5.exe"
    )
  } else {
    # case os is not windows
    .get_exec_on_linux_or_mac(
      path = c("/usr/local/bin", "/usr/bin"), exe = "swmm5"
    )
  }
  
  # set option
  op.swmmr <- list(
    swmmr.exec = swmmr.exec
  )
  
  # get the difference to available options
  toset <- !(names(op.swmmr) %in% names(op))
  # save options
  if (any(toset)) options(op.swmmr[toset])
  
  invisible()
}