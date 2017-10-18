#' Convert swmm objects to simple feature geometries
#'
#' * `junctions_to_sf()`: converts junctions to simple features
#' * `links_to_sf()`: converts links to simple features
#' * `subcatchments_to_sf()`: converts subcatchments to simple features
#' * `inp_to_sf()`: converts junctions, links and subcatchments to simple features 
#' and returns a list
#'
#' @param x An object of class 'swmm_inp', created by \code{\link{read_inp}}.
#' @return A simple feature or a list of simple features
#' @name convert_to_sf
#' @seealso \code{\link[ggplot2]{geom_sf}}
NULL

#' @export
#' @rdname convert_to_sf
junctions_to_sf <- function(x) {
  
  # check class
  stopifnot(class(x) == "swmm_inp")
  
  # join junctions and coordinates
  junc_sf <- dplyr::left_join(x = x[["junctions"]],
                              y = x[["coordinates"]],
                              by = c("Name" = "Node")) %>%
    # nest by coordinates
    tidyr::nest(`X-Coord`,`Y-Coord`, .key = "geometry") %>% 
    # create multi per subcatchment
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>% 
                                          sf::st_point(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry)) %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(junc_sf)
  
}

#' @export
#' @rdname convert_to_sf
subcatchments_to_sf <- function(x) {

  # check class
  stopifnot(class(x) == "swmm_inp")
  
  # join subcatchment, subareas, infiltration and polygons
  subc_sf <- dplyr::left_join(x = x[["subcatchments"]],
                              y = x[["subareas"]],
                              by = c("Name" = "Subcatchment")) %>% 
    dplyr::left_join(x = ., 
                     y = x[["infiltration"]], 
                     by = c("Name" = "Subcatchment")) %>% 
    dplyr::left_join(x = ., 
                     y = x[["polygons"]], 
                     by = c("Name" = "Subcatchment")) %>% 
    # nest by coordinates
    tidyr::nest(`X-Coord`,`Y-Coord`, .key = "geometry") %>% 
    # create polygon per subcatchment
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>% 
                                          list(.) %>%
                                          sf::st_polygon(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry)) %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(subc_sf)

}

#' @export
#' @rdname convert_to_sf
links_to_sf <- function(x) {
  
  # check class
  stopifnot(class(x) == "swmm_inp")
  
  # join junctions and coordinates
  links_sf <- dplyr::left_join(x = x[["conduits"]],
                               y = x[["vertices"]],
                              by = c("Name" = "Link")) %>%
    # nest by coordinates
    tidyr::nest(`X-Coord`,`Y-Coord`, .key = "geometry") %>% 
    # create multi per subcatchment
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>%
                                          sf::st_linestring(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry)) %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(links_sf)
  
}

#' @export
#' @rdname convert_to_sf
inp_to_sf <- function(x) {
  
  # check class
  stopifnot(class(x) == "swmm_inp")
  
  # return list with simple features of swmm objects
  sf <- list(subcatchments = subcatchments_to_sf(x), 
             junctions = junctions_to_sf(x),
             links = links_to_sf(x))
    
  return(sf)
}