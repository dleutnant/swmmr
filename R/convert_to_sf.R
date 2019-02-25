#' Convert swmm objects to simple feature geometries
#'
#' * `junctions_to_sf()`: converts junctions to simple features (required 
#' sections: `junctions` and `coordinates`)
#' * `outfalls_to_sf()`: converts junctions to simple features (required 
#' sections: `outfalls` and `coordinates`)
#' * `links_to_sf()`: converts links to simple features (required sections:
#' `conduits` and `coordinates`)
#' * `subcatchments_to_sf()`: converts subcatchments to simple features (required 
#' sections: `subcatchments`, `subareas`, `infiltration` and `polygons`)
#' * `raingages_to_sf() `: converts raingages to simple features (required 
#' sections: `raingages` and `symbols`)
#' * `storages_to_sf()`: converts storages to simple features (required sections:
#' `storage` and `coordinates`)
#' * `weirs_to_sf()`: converts weirs to simple features (required sections:
#' `weirs` and `coordinates`)
#' * `orifices_to_sf()`: converts orifices to simple features (required sections:
#' `orifices` and `coordinates`)
#' * `pumps_to_sf()`: converts pumps to simple features (required sections:
#' `pumps` and `coordinates`)
#' * `inp_to_sf()`: converts junctions, outfalls, links, storages, weirs, 
#' orifices, pumps, subcatchments and raingages to a list of simple features
#'
#' @param x An object of class 'inp', created by \code{\link{read_inp}}.
#' @param remove_invalid Should invalid sf geometries be removed?
#' @return A simple feature or a list of simple features
#' @name convert_to_sf
#' @seealso \code{\link[sf]{sf}}
NULL

#' @export
#' @rdname convert_to_sf
raingages_to_sf <- function(x) {

  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "raingages", c("raingages", "symbols"))) {
    return(NULL)
  } 
  
  # return simple feature objects of raingages
  rg_sf <- dplyr::left_join(x = x[["raingages"]],
                              y = x[["symbols"]],
                              by = c("Name" = "Gage")) %>% 
    create_sf_of_pt()
}

#' Helper function
#' @keywords internal
check_package_and_class <- function(x, package = "sf", class = "inp") {
  
  # checks if sf is available
  check_pkg_avail(package)
  
  # check class and required elements
  stopifnot(inherits(x, class))
}

#' Helper function
#' @keywords internal
has_incomplete_features <- function(x, subject, features) {
  
  incomplete <- ! all(features %in% names(x))
  
  if (incomplete) {
    warning("incomplete features: ", subject)
  } 

  incomplete
}

#' @export
#' @rdname convert_to_sf
junctions_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "junctions", c("junctions", "coordinates"))) {
    return(NULL)
  } 
  
  # return simple feature objects of junctions
  dplyr::left_join(x = x[["junctions"]],
                   y = x[["coordinates"]],
                   by = c("Name" = "Node")) %>% 
    create_sf_of_pt()
  
}

#' @export
#' @rdname convert_to_sf
outfalls_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "outfalls", c("outfalls", "coordinates"))) {
    return(NULL)
  } 
  
  # return simple feature objects of outfalls
  outf_sf <- dplyr::left_join(x = x[["outfalls"]],
                              y = x[["coordinates"]],
                              by = c("Name" = "Node")) %>% 
    create_sf_of_pt()

}

#' @export
#' @rdname convert_to_sf
storages_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "storage", c("storage", "coordinates"))) {
    return(NULL)
  } 
  
  # return simple feature objects of storages
  storages_sf <- dplyr::left_join(x = x[["storage"]],
                                  y = x[["coordinates"]],
                                  by = c("Name" = "Node"))  %>% 
    create_sf_of_pt()
  
}

#' @export
#' @rdname convert_to_sf
subcatchments_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "subcatchments", c(
    "subcatchments", "subareas", "infiltration", "polygons"
  ))) {
    return(NULL)
  } 
  
  # join subcatchment, subareas, infiltration and polygons
  subc_sf <- dplyr::left_join(x = x[["subcatchments"]],
                              y = x[["subareas"]],
                              by = c("Name" = "Subcatchment")) %>% 
    dplyr::left_join(x = ., 
                     y = x[["infiltration"]], 
                     by = c("Name" = "Subcatchment")) %>% 
    dplyr::left_join(x = ., 
                     y = x[["polygons"]], 
                     by = c("Name" = "Subcatchment")) 
  
  # add lids if available
  if ("lid_usage" %in% names(x)) {
    subc_sf <- dplyr::left_join(x = subc_sf,
                                y = x[["lid_usage"]],
                                by = c("Name" = "Subcatchment"),
                                suffix = c(".subcatchment", ".lid_usage"))
  }
    
  subc_sf <- subc_sf %>% 
    # nest by coordinates
    tidyr::nest(`X-Coord`,`Y-Coord`, .key = "geometry") %>%
    # remove geometries with less than 3 points
    dplyr::filter(purrr::map_lgl(geometry, ~ nrow(.)>2)) %>% 
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
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "links", c("conduits", "coordinates"))) {
    return(NULL)
  } 
  
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
    links_df <- dplyr::left_join(x = links_df,
                                 y = x[["xsections"]],
                                 by = c("Name" = "Link"))
  }
  
  # extract losses if available
  if ("losses" %in% names(x)) {
    links_df <- dplyr::left_join(x = links_df,
                                 y = x[["losses"]],
                                 by = c("Name" = "Link"))
  }
  
  # return simple feature objects of links
  create_sf_of_linestring(links_df)
  
}

#' @export
#' @rdname convert_to_sf
weirs_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features("weirs", c("weirs", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["weirs"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["weirs"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  weirs_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["weirs"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    weirs_df <- dplyr::bind_rows(weirs_df, vertices)
  }
  
  # return simple feature objects of links
  create_sf_of_linestring(weirs_df)

}

#' @export
#' @rdname convert_to_sf
orifices_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "orifices", c("orifices", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["orifices"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["orifices"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  orifices_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["orifices"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    orifices_df <- dplyr::bind_rows(orifices_df, vertices)
  }
  
  # return simple feature objects of orifices
  create_sf_of_linestring(orifices_df)
  
}

#' @export
#' @rdname convert_to_sf
pumps_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "pumps", c("pumps", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["pumps"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["pumps"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  pumps_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["pumps"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    pumps_df <- dplyr::bind_rows(pumps_df, vertices)
  }
  
  # return simple feature objects of pumps
  create_sf_of_linestring(pumps_df)

}

#' @export
#' @rdname convert_to_sf
weirs_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "weirs", c("weirs", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["weirs"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["weirs"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  weirs_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["weirs"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    weirs_df <- dplyr::bind_rows(weirs_df, vertices)
  }
  
  weirs_df <- weirs_df %>% 
    # sort by pos and id to maintain structure
    dplyr::arrange(pos, id)
  
  # create df with data only
  data <- weirs_df %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- weirs_df %>% 
    dplyr::select(Name, `X-Coord`, `Y-Coord`) %>% 
    tidyr::nest(`X-Coord`, `Y-Coord`, .key = "geometry") %>% 
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>%
                                          sf::st_linestring(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry))
  
  # join data and sf column
  weirs_sf <- dplyr::left_join(data, sf, by = "Name") %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(weirs_sf)
  
}

#' @export
#' @rdname convert_to_sf
orifices_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "orifices", c("orifices", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["orifices"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["orifices"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  orifices_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["orifices"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    orifices_df <- dplyr::bind_rows(orifices_df, vertices)
  }
  
  orifices_df <- orifices_df %>% 
    # sort by pos and id to maintain structure
    dplyr::arrange(pos, id)
  
  # create df with data only
  data <- orifices_df %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- orifices_df %>% 
    dplyr::select(Name, `X-Coord`, `Y-Coord`) %>% 
    tidyr::nest(`X-Coord`, `Y-Coord`, .key = "geometry") %>% 
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>%
                                          sf::st_linestring(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry))
  
  # join data and sf column
  orifices_sf <- dplyr::left_join(data, sf, by = "Name") %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(orifices_sf)
  
}

#' @export
#' @rdname convert_to_sf
pumps_to_sf <- function(x) {
  
  check_package_and_class(x)
  
  # check sections
  if (has_incomplete_features(x, "pumps", c("pumps", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start_nodes
  start_node <- dplyr::left_join(x = x[["pumps"]], 
                                 y = x[["coordinates"]], 
                                 by = c("From Node" = "Node")) %>% 
    dplyr::mutate(pos = 1L, 
                  id = 1L)
  
  # extract end_nodes
  end_node <- dplyr::left_join(x = x[["pumps"]], 
                               y = x[["coordinates"]], 
                               by = c("To Node" = "Node")) %>% 
    dplyr::mutate(pos = 3L, 
                  id = 1L)
  
  # bind dfs
  pumps_df <- dplyr::bind_rows(start_node, end_node)
  
  # extract vertices if available
  if ("vertices" %in% names(x)) {
    vertices <- dplyr::inner_join(x = x[["pumps"]],
                                  y = x[["vertices"]],
                                  by = c("Name" = "Link")) %>% 
      dplyr::mutate(pos = 2L) %>% 
      dplyr::group_by(Name) %>% 
      dplyr::mutate(id = seq_along(Name)) %>% 
      dplyr::ungroup()
    
    # add vertices
    pumps_df <- dplyr::bind_rows(pumps_df, vertices)
  }
  
  pumps_df <- pumps_df %>% 
    # sort by pos and id to maintain structure
    dplyr::arrange(pos, id)
  
  # create df with data only
  data <- pumps_df %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- pumps_df %>% 
    dplyr::select(Name, `X-Coord`, `Y-Coord`) %>% 
    tidyr::nest(`X-Coord`, `Y-Coord`, .key = "geometry") %>% 
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>%
                                          sf::st_linestring(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry))
  
  # join data and sf column
  pumps_sf <- dplyr::left_join(data, sf, by = "Name") %>% 
    # create simple feature objects
    sf::st_sf()
  
  # return simple feature objects of subcatchments
  return(pumps_sf)
  
}


#' @export
#' @rdname convert_to_sf
inp_to_sf <- function(x, remove_invalid = TRUE) {
  
  check_package_and_class(x)
  
  # return list with simple features of swmm objects
  sf <- list(subcatchments = subcatchments_to_sf(x), 
             junctions = junctions_to_sf(x),
             outfalls = outfalls_to_sf(x),
             storages = storages_to_sf(x),
             links = links_to_sf(x), 
             weirs = weirs_to_sf(x),
             orifices = orifices_to_sf(x),
             pumps = pumps_to_sf(x),
             raingages = raingages_to_sf(x))
  
  # discard nulls
  sf <- purrr::discard(sf, is.null)
  
  # should invalid sf geometries be removed?
  if (remove_invalid) {
    
    # get index of invalid sf geometries
    list_of_idx <- purrr::map(sf, sf::st_is_valid)
    
    # raise warning in case of invalid geometries
    purrr::iwalk(list_of_idx,  ~ {
      if (anyNA(.x)) warning(paste("removing invalid geometries of", .y))
    })
    
    # remove invalid geometries
    sf <- purrr::map2(sf, list_of_idx,  ~ .x[!is.na(.y), ])
    
  }
  
  return(sf)
}

#' helper function to create a simple feature object from point geometry
#' function is not exported
#' 
#' @param x a tibble to be converted
#' @keywords internal
create_sf_of_pt <- function(x) {
  
  # nest by coordinates
  tidyr::nest(x, `X-Coord`,`Y-Coord`, .key = "geometry") %>% 
    # create points
    dplyr::mutate(geometry = purrr::map(geometry,
                                        ~ data.matrix(.) %>% 
                                          sf::st_point(.))) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry)) %>% 
    # create simple feature objects
    sf::st_sf()
  
}

#' helper function to create a simple feature object from linestring geometry
#' function is not exported
#' 
#' @param x a tibble to be converted
#' @keywords internal
create_sf_of_linestring <- function(x) {
  
  # sort by pos and id to maintain structure
  x <- dplyr::arrange(x, pos, id)
  
  # create df with data only
  data <- x %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- x %>% 
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
  
}
