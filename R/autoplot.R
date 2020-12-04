#' @title Plot a swmm model structure using ggplot2
#' @description This function reads an object of class 'inp'. All objects are
#' converted to simple feature geometries via \code{\link[swmmr]{inp_to_sf}} and 
#' finally passed to \code{\link[ggplot2]{geom_sf}}. It allows to quickly 
#' visualize a model structure.
#' @param x An inp object
#' @param ... currently ignored
#' @note Lifecycle: experimental
#' @examples
#' \dontrun{
#' inp <- read_inp("model.inp")
#' autoplot(inp)
#' }
#' @rdname plot
#' @export
autoplot.inp <- function(x, ...) {

  # checks if ggplot2 is available
  check_pkg_avail("ggplot2")

  # create list with simple feature objects for
  # subcatchments, junctions, links and raingages
  sff <- inp_to_sf(x)

  # init ggplot obj
  p <- ggplot2::ggplot()

  # walk through the list
  for (i in seq_along(sff)) {
    # create extra shape for raingages
    if ("raingages" != names(sff)[i]) {
      p <- p + ggplot2::geom_sf(data = sff[[i]])
    } else {
      p <- p + ggplot2::geom_sf(
        data = sff[[i]],
        shape = 10,
        show.legend = FALSE
      )
    }
  }
  return(p)
}