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
      infiltration = infiltration, 
      subcatchment = subcatchment, 
      subcatchment_typologies = subcatchment_typologies, 
      conduit_material = conduit_material, 
      junction_parameters = junction_parameters
    )) %>%
    # Reclass to tibbles for consistency
    purrr::map( ~ set_class(.x, c("tbl_df", "tbl", "data.frame")))
  
  # Select/order sections
  # TODO: warn about sections that get lost here!
  result <- result[intersect(get_section_names_for_input(), names(result))]
  
  # assign class attribute
  set_class(result, "inp")
}
