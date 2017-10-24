#' @title Plot a swmm model structure
#' @description This function reads an object of class 'inp' and extracts
#' subcatchments, links and junctions. All objects are converted to simple feautures
#' geometries and passed to \code{\link[ggplot2]{geom_sf}}.
#' @param x An inp object
#' @param ... currently ignored
#' @examples  
#' \dontrun{
#' inp <- read_inp("model.inp")
#' plot(inp)
#' }
#' @rdname plot
#' @export
plot.inp <- function(x,...) {
  
  # create list with simple feature objects for 
  # subcatchments, junctions and nodes
  sff <- inp_to_sf(x) 
  ggplot2::ggplot() + 
    ggplot2::geom_sf(data = sff$subcatchments) + 
    ggplot2::geom_sf(data = sff$links) + 
    ggplot2::geom_sf(data = sff$junctions)
  
}
