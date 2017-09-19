nodes_to_sf <- function(x) {
  
  x[["COORDINATES"]] %>%
    .[, c("X-Coord", "Y-Coord")] %>% 
    as.matrix() %>% 
    sf::st_multipoint()
  
}

subcatchments_to_sf <- function(x) {
  
  x[["Polygons"]] %>%
    split(.$Subcatchment) %>% 
    lapply(function(x) as.matrix(x[, c("X-Coord", "Y-Coord")])) %>% 
    sf::st_polygon()
  
}

links_to_sf <- function(x) {
  
  x[["VERTICES"]] %>%
    split(.$Link) %>% 
    lapply(function(x) as.matrix(x[, c("X-Coord", "Y-Coord")])) %>% 
    sf::st_multilinestring()
  
}
