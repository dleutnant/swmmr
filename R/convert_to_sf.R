#' Convert swmm objects to simple feature geometries
#'
#' * `junctions_to_sf()`: converts junctions to simple features (required 
#' sections: `junctions` and `coordinates`)
#' * `links_to_sf()`: converts links to simple features (required sections:
#' `conduits` and `coordinates`)
#' * `subcatchments_to_sf()`: converts subcatchments to simple features (required 
#' sections: `subcatchments`, `subareas` and `infiltration`)
#' * `inp_to_sf()`: converts junctions, links and subcatchments to simple features 
#' and returns a list
#'
#' @param x An object of class 'inp', created by \code{\link{read_inp}}.
#' @return A simple feature or a list of simple features
#' @name convert_to_sf
#' @seealso \code{\link[ggplot2]{geom_sf}}
NULL

#' @export
#' @rdname convert_to_sf
junctions_to_sf <- function(x) {
  
  # check class and required elements
  stopifnot(inherits(x, "inp"), 
            "junctions" %in% names(x),
            "coordinates" %in% names(x))
  
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

  # check class and required elements
  stopifnot(inherits(x, "inp"), 
            "subcatchments" %in% names(x), 
            "subareas" %in% names(x),
            "infiltration" %in% names(x))
  
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
    # check if polygon is closed
    dplyr::mutate(polygon_is_closed = purrr::map_lgl(geometry,
                                                     ~ identical(head(., 1),
                                                                 tail(., 1)))) %>%
    # close polygon
    dplyr::mutate(geometry = ifelse(polygon_is_closed, 
                                    geometry, 
                                    purrr::map(geometry, 
                                               ~ dplyr::bind_rows(., head(., 1))))) %>% 
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
  stopifnot(inherits(x, "inp"), 
            "conduits" %in% names(x),
            "coordinates" %in% names(x))
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["conduits"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["conduits"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  links_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["conduits"]],
                                 y = x[["vertices"]],
                                 by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    links_df <- dplyr::bind_rows(links_df, vertices)
  }
  
  # extract xsections if available
  if ("xsections" %in% names(x)) {
    links_df <- dplyr::inner_join(x = links_df,
                                  y = x[["xsections"]],
                                  by = c("Name" = "Link"))
  }

  links_df <- links_df %>% 
    # sort by pos and id to maintain structure
    dplyr::arrange(pos, id)
  
  # create df with data only
  data <- links_df %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- links_df %>% 
    dplyr::select(Name, `X-Coord`, `Y-Coord`) %>% 
    tidyr::nest(`X-Coord`, `Y-Coord`, .key = "geometry") %>% 
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>%
                                          sf::st_linestring(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry))
  
  # join data and sf column
  links_sf <- dplyr::left_join(data, sf, by = "Name") %>% 
    # create simple feature objects
    sf::st_sf()

  # return simple feature objects of subcatchments
  return(links_sf)
  
}

#' @export
#' @rdname convert_to_sf
inp_to_sf <- function(x) {
  
  # check class
  stopifnot(inherits(x, "inp"))
  
  # return list with simple features of swmm objects
  sf <- list(subcatchments = subcatchments_to_sf(x), 
             junctions = junctions_to_sf(x),
             links = links_to_sf(x))
    
  return(sf)
}