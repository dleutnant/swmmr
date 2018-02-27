#' #' @title Plot a swmm model structure
#' #' @description This function reads an object of class 'inp' and extracts
#' #' subcatchments, links, junctions and raingages. All objects are converted to simple feautures
#' #' geometries and passed to \code{\link[ggplot2]{geom_sf}}.
#' #' @param x An inp object
#' #' @param ... currently ignored
#' #' @examples
#' #' \dontrun{
#' #' inp <- read_inp("model.inp")
#' #' plot(inp)
#' #' }
#' #' @rdname plot
#' #' @export
#' plot.inp <- function(x, ...) {
#' 
#'   # checks if ggplot2 is available
#'   check_pkg_avail("ggplot2")
#' 
#'   # create list with simple feature objects for
#'   # subcatchments, junctions, links and raingages
#'   sff <- inp_to_sf(x)
#' 
#'   # init ggplot obj
#'   p <- ggplot2::ggplot()
#' 
#'   # walk through the list
#'   for (i in 1:length(sff)) {
#'     # create extra shape for raingages
#'     if ("raingages" != names(sff)[i]) {
#'       p <- p + ggplot2::geom_sf(data = sff[[i]])
#'     } else {
#'       p <- p + ggplot2::geom_sf(
#'         data = sff[[i]],
#'         shape = 10,
#'         show.legend = FALSE
#'       )
#'     }
#'   }
#'   return(p)
#' }