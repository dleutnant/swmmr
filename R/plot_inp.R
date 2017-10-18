#' @title Plot a swmm model structure
#' @description This function reads an object of class 'swmm_inp' and extracts
#' subcatchments, links and junctions. All objects are converted to simple feautures
#' geometries and passed to ggplot2::geom_sf.
#' @param x An swmm_inp object
#' @param ... currently ignored
#' @examples  
#' \dontrun{
#' swmm_inp <- read_inp("model.inp")
#' plot(swmm_inp)
#' }
#' @rdname plot.swmm_inp
#' @export
plot.swmm_inp <- function(x,...) {
  
  # create list with simple feature objects for 
  # subcatchments, junctions and nodes
  sff <- inp_to_sf(x) 
  ggplot2::ggplot() + 
    ggplot2::geom_sf(data = sff$subcatchments) + 
    ggplot2::geom_sf(data = sff$links) + 
    ggplot2::geom_sf(data = sff$junctions) + 
    ggplot2::theme_bw()  
  
}
