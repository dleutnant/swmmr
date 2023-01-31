#' Convert swmm objects to simple feature geometries
#'
#' * `junctions_to_sf()`: converts junctions to simple features 
#'   (required sections: `junctions` and `coordinates`)
#' * `outfalls_to_sf()`: converts junctions to simple features 
#'   (required sections: `outfalls` and `coordinates`)
#' * `links_to_sf()`: converts links to simple features 
#'   (required sections: `conduits` and `coordinates`)
#' * `subcatchments_to_sf()`: converts subcatchments to simple features 
#'   (required sections: `subcatchments`, `subareas`, `infiltration` and 
#'   `polygons`)
#' * `raingages_to_sf() `: converts raingages to simple features 
#'   (required sections: `raingages` and `symbols`)
#' * `storages_to_sf()`: converts storages to simple features 
#'   (required sections: `storage` and `coordinates`)
#' * `weirs_to_sf()`: converts weirs to simple features 
#'   (required sections: `weirs` and `coordinates`)
#' * `orifices_to_sf()`: converts orifices to simple features 
#'   (required sections: `orifices` and `coordinates`)
#' * `pumps_to_sf()`: converts pumps to simple features 
#'   (required sections: `pumps` and `coordinates`)
#' * `inp_to_sf()`: converts junctions, outfalls, links, storages, weirs,
#'   orifices, pumps, subcatchments and raingages to a list of simple features
#'
#' @param x An object of class 'inp', created by \code{\link{read_inp}}.
#' @param remove_invalid Should invalid sf geometries be removed?
#' @return A simple feature or a list of simple features
#' @name convert_to_sf
#' @seealso \code{\link[sf]{sf}}
NULL

#' @export
#' @rdname convert_to_sf
raingages_to_sf <- function(x)
{
  left_joined_to_sf_points_or_null(x, "raingages", "symbols", "Gage")
}

#' @export
#' @rdname convert_to_sf
junctions_to_sf <- function(x)
{
  left_joined_to_sf_points_or_null(x, "junctions", "coordinates", "Node")
}

#' @export
#' @rdname convert_to_sf
outfalls_to_sf <- function(x)
{
  left_joined_to_sf_points_or_null(x, "outfalls", "coordinates", "Node")
}

#' @export
#' @rdname convert_to_sf
storages_to_sf <- function(x)
{
  left_joined_to_sf_points_or_null(x, "storage", "coordinates", "Node")
}

#' Helper function
#' @keywords internal
left_joined_to_sf_points_or_null <- function(x, left, right, by_y)
{
  if (!check_package_class_features(x, c(left, right))) {
    return(NULL)
  }

  x[[left]] %>%
    dplyr::left_join(x[[right]], by = c(Name = by_y)) %>% 
    create_sf_of_pt()
}

#' @export
#' @rdname convert_to_sf
subcatchments_to_sf <- function(x)
{
  required <- c("subcatchments", "subareas", "infiltration", "polygons")
  
  if (!check_package_class_features(x, required)) {
    return(NULL)
  } 
  
  # join subcatchment, subareas, infiltration and polygons
  by <- c("Name" = "Subcatchment")
  
  subc_sf <- x[["subcatchments"]] %>%
    dplyr::left_join(x[["subareas"]], by = by) %>% 
    dplyr::left_join(x[["infiltration"]], by = by) %>% 
    dplyr::left_join(x[["polygons"]], by = by) 
  
  # add lids if available
  if ("lid_usage" %in% names(x)) {
    subc_sf <- dplyr::left_join(
      subc_sf,
      x[["lid_usage"]],
      by = by,
      suffix = c(".subcatchment", ".lid_usage")
    )
  }
  
  # return simple feature objects of subcatchments...
  subc_sf %>% 
    # nest by coordinates
    tidyr::nest(geometry = c(`X-Coord`, `Y-Coord`)) %>%
    # remove geometries with less than 3 points
    dplyr::filter(purrr::map_lgl(geometry, ~ nrow(.)>2)) %>% 
    # check if polygon is closed
    dplyr::mutate(polygon_is_closed = purrr::map_lgl(
      geometry, ~ identical(head(., 1), tail(., 1))
    )) %>%
    # close polygon
    dplyr::mutate(geometry = ifelse(
      polygon_is_closed, 
      geometry, 
      purrr::map(geometry, ~ dplyr::bind_rows(., head(., 1)))
    )) %>% 
    # create polygon per subcatchment
    dplyr::mutate(geometry = purrr::map(
      geometry, ~ data.matrix(.) %>% 
        list(.) %>% 
        sf::st_polygon(.)
    )) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry)) %>% 
    # create simple feature objects
    sf::st_sf()
}

#' @export
#' @rdname convert_to_sf
links_to_sf <- function(x)
{
  features <- c("conduits", "coordinates")
  
  if (!check_package_class_features(x, features, subject = "links")) {
    return(NULL)
  } 
  
  # extract start and end nodes, extract vertices if available
  links_df <- x[["conduits"]] %>%
    extract_start_and_end_nodes(x[["coordinates"]]) %>%
    extract_vertices_if_available(x, "conduits")
  
  # by argument for the following joins
  by <- c("Name" = "Link")
  
  # extract xsections if available
  if ("xsections" %in% names(x)) {
    links_df <- dplyr::left_join(links_df, x[["xsections"]], by = by)
  }
  
  # extract losses if available
  if ("losses" %in% names(x)) {
    links_df <- dplyr::left_join(links_df, x[["losses"]], by = by)
  }
  
  # return simple feature objects of links
  create_sf_of_linestring(links_df)
}

#' Helper function
#' @keywords internal
extract_start_and_end_nodes <- function(data, coordinates)
{
  extract_nodes <- function(by_x, pos) {
    data %>%
      dplyr::left_join(coordinates, by = stats::setNames("Node", by_x)) %>% 
      dplyr::mutate(pos = pos, id = 1L)
  }
  
  dplyr::bind_rows(
    extract_nodes("From Node", 1L), 
    extract_nodes("To Node", 3L)
  )
}

#' Helper function
#' @keywords internal
extract_vertices_if_available <- function(data, x, name)
{
  if (!("vertices" %in% names(x))) {
    return(data)
  }
  
  vertices <- x[[name]] %>%
    dplyr::inner_join(x[["vertices"]], by = c(Name = "Link")) %>% 
    dplyr::mutate(pos = 2L) %>% 
    dplyr::group_by(Name) %>% 
    dplyr::mutate(id = seq_along(Name)) %>% 
    dplyr::ungroup()
  
  # add vertices
  dplyr::bind_rows(data, vertices)
}

#' @export
#' @rdname convert_to_sf
weirs_to_sf <- function(x)
{
  if (!check_package_class_features(x, c("weirs", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start and end nodes, extract vertices if available, return simple 
  # feature objects of links
  x[["weirs"]] %>%
    extract_start_and_end_nodes(x[["coordinates"]]) %>%
    extract_vertices_if_available(x, "weirs") %>%
    create_sf_of_linestring()
}

#' @export
#' @rdname convert_to_sf
orifices_to_sf <- function(x)
{
  if (!check_package_class_features(x, c("orifices", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start and end nodes, extract vertices if available, return simple 
  # feature objects of orifices
  x[["orifices"]] %>% 
    extract_start_and_end_nodes(x[["coordinates"]]) %>%
    extract_vertices_if_available(x, "orifices") %>%
    create_sf_of_linestring()
}

#' @export
#' @rdname convert_to_sf
pumps_to_sf <- function(x)
{
  if (!check_package_class_features(x, c("pumps", "coordinates"))) {
    return(NULL)
  } 
  
  # extract start and end nodes, extract vertices if available, return simple 
  # feature objects of pumps
  x[["pumps"]] %>%
    extract_start_and_end_nodes(x[["coordinates"]]) %>%
    extract_vertices_if_available(x, "pumps") %>%
    create_sf_of_linestring()
}

#' Helper function
#' @keywords internal
check_package_class_features <- function(x, features, ...)
{
  check_package_and_class(x)
  
  !has_incomplete_features(x, features, ...)
}

#' Helper function
#' @keywords internal
check_package_and_class <- function(x, package = "sf", class = "inp")
{
  # checks if sf is available
  check_pkg_avail(package)
  
  # check class and required elements
  if (!inherits(x, class)) {
    stop_formatted(
      "%s does not inherit from class '%s' as expected.",
      deparse(substitute(x, env = parent.frame(n = 1L))), class
    )
  }
}

#' Helper function
#' @keywords internal
has_incomplete_features <- function(x, features, subject = features[1L])
{
  is_there <- features %in% names(x)
  is_incomplete <- !all(is_there)
  
  if (is_incomplete) {
    warn_formatted(
      "incomplete features: %s (missing: %s)", 
      subject, comma_space_collapsed(features[!is_there])
    )
  } 
  
  is_incomplete
}

#' @export
#' @rdname convert_to_sf
inp_to_sf <- function(x, remove_invalid = TRUE)
{
  check_package_and_class(x)
  
  # return list with simple features of swmm objects
  sf <- list(
    subcatchments = subcatchments_to_sf(x), 
    junctions = junctions_to_sf(x),
    outfalls = outfalls_to_sf(x),
    storages = storages_to_sf(x),
    links = links_to_sf(x), 
    weirs = weirs_to_sf(x),
    orifices = orifices_to_sf(x),
    pumps = pumps_to_sf(x),
    raingages = raingages_to_sf(x)
  )
  
  # discard nulls
  sf <- purrr::discard(sf, is.null)
  
  # should invalid sf geometries be removed?
  if (remove_invalid) {
    
    # get index of invalid sf geometries
    list_of_idx <- purrr::map(sf, sf::st_is_valid)
    
    # raise warning in case of invalid geometries
    purrr::iwalk(list_of_idx,  ~ {
      if (anyNA(.x)) {
        warning("removing invalid geometries of ", .y)
      }
    })
    
    # remove invalid geometries
    sf <- purrr::map2(sf, list_of_idx,  ~ .x[!is.na(.y), ])
  }
  
  sf
}

#' helper function to create a simple feature object from point geometry
#' function is not exported
#' 
#' @param x a tibble to be converted
#' @keywords internal
create_sf_of_pt <- function(x)
{
  # nest by coordinates
  tidyr::nest(x, geometry = c(`X-Coord`, `Y-Coord`)) %>%
    # create points
    dplyr::mutate(geometry = purrr::map(
      geometry,
      ~ data.matrix(.) %>% 
        sf::st_point(.)
    )) %>% 
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
create_sf_of_linestring <- function(x)
{
  # sort by pos and id to maintain structure
  x <- dplyr::arrange(x, pos, id)
  
  # create df with data only
  data <- x %>% 
    dplyr::select(-`X-Coord`, -`Y-Coord`, -pos, -id ) %>%
    dplyr::distinct(.keep_all = TRUE)
  
  # create df with sf column
  sf <- x %>% 
    dplyr::select(Name, `X-Coord`, `Y-Coord`) %>% 
    tidyr::nest(geometry = c(`X-Coord`, `Y-Coord`)) %>%
    dplyr::mutate(geometry = purrr::map(
      geometry,
      ~ data.matrix(.) %>%
        sf::st_linestring(.)
    )) %>% 
    # create geometry column
    dplyr::mutate(geometry = sf::st_sfc(geometry))
  
  # join data and sf column
  dplyr::left_join(data, sf, by = "Name") %>% 
    # create simple feature objects
    sf::st_sf()
}
