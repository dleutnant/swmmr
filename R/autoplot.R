#' @title Plot a swmm model structure using ggplot2
#' @description This function reads an object of class 'inp'. All objects are
#' converted to simple feature geometries via \code{\link[swmmr]{inp_to_sf}} and 
#' finally passed to \code{\link[ggplot2]{geom_sf}}. It allows to quickly 
#' visualize a model structure.
#' @param object An inp object
#' @param ... currently ignored
#' @note Lifecycle: experimental
#' @importFrom ggplot2 autoplot
#' @examples
#' \dontrun{
#' inp <- read_inp("model.inp")
#' autoplot(inp)
#' }
#' @rdname plot
#' @export
autoplot.inp <- function(object, ...)
{
  # checks if ggplot2 is available
  check_pkg_avail("ggplot2")
  
  # create list with simple feature objects for
  # subcatchments, junctions, links and raingages
  simple_features <- inp_to_sf(object)
  
  # init ggplot obj
  p <- ggplot2::ggplot()
  
  # walk through the list
  for (name in names(simple_features)) {
    
    data <- simple_features[[name]]
    
    # create extra shape for raingages
    p <- p + if (name == "raingages") {
      ggplot2::geom_sf(data = data, shape = 10, show.legend = FALSE)
    } else {
      ggplot2::geom_sf(data = data)
    }
  }
  
  p
}
