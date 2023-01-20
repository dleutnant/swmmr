#' Reads input data which was checked for completeness and converts/processes
#' them to an object of class inp.
#'
#' @keywords internal 
list_of_sections_to_inp <- function(
    list_of_sections, 
    infiltration, 
    subcatchment_typologies, 
    conduit_material, 
    junction_parameters
)
{
  subcatchment <- list_of_sections[["subcatchments"]]
  
  # ...further processing of entries in list_of_sections...
  res <- list_of_sections %>%
    # define classes
    purrr::imap(function(.x, .y) {
      class(.x) <- c(.y, class(.x))
      return(.x)
    }) %>%
    # assign section parameters individually
    purrr::map(., ~ assign_parameters(
      .x, 
      infiltration, 
      subcatchment, 
      subcatchment_typologies, 
      conduit_material, 
      junction_parameters
    )) %>%
    # reclass to tibbles for consistency
    purrr::map( ~ {
      class(.x) <- c("tbl_df", "tbl", "data.frame")
      .x
    })
  
  # adjust order of sections
  section_order <-  c(
    "title", 
    "options", 
    "evaporation", 
    "raingages", 
    "subcatchments",
    "subareas", 
    "infiltration", 
    "aquifers", 
    "groundwater", 
    "LID_controls", 
    "LID_usage",
    "junctions", 
    "outfalls", 
    "storage", 
    "conduits", 
    "pumps", 
    "weirs", 
    "xsections", 
    "controls", 
    "DWF", 
    "pollutants", 
    "landuses", 
    "coverages", 
    "loadings", 
    "buildup", 
    "washoff", 
    "inflows", 
    "timeseries", 
    "curves", 
    "patterns",
    "report", 
    "tags", 
    "map", 
    "coordinates", 
    "vertices", 
    "polygons", 
    "labels", 
    "symbols", 
    "backdrop"
  )
  
  res <- res[section_order[section_order %in% names(res)]]
  
  # assign class attribute
  class(res) <- "inp"
  
  res
}
