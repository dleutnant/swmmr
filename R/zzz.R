# inspired by http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname)
{
  # load options available
  op <- options()
  
  # get path to swmm executable
  swmmr.exec <- .get_exec()
  
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
