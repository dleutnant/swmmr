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
  result <- list_of_sections %>%
    # Set class according to the name of the element in the list
    purrr::imap(add_class) %>%
    # Assign section parameters individually
    purrr::map(., ~ assign_parameters(
      .x, 
      infiltration, 
      subcatchment, 
      subcatchment_typologies, 
      conduit_material, 
      junction_parameters
    )) %>%
    # Reclass to tibbles for consistency
    purrr::map( ~ set_class(.x, c("tbl_df", "tbl", "data.frame")))
  
  # adjust order of sections
  section_order <- get_section_order_for_input()
  
  result <- result[section_order[section_order %in% names(result)]]
  
  # assign class attribute
  set_class(result, "inp")
}

# get_section_order_for_input ---------------------------------------------------
get_section_order_for_input <- function()
{
  sections <- c(
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

  # Instead: read section names from "sections.csv"  
  info <- swmmr:::section_info()
  info <- info[!is.na(info$input), ]
  
  # Order section names by number in column "input" first
  # and by the section name second
  sections2 <- info$section[order(info$input, info$section)]
  
  stopifnot(identical(section2, tolower(sections)))
  
  sections
}
