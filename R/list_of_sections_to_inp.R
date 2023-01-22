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
  
  # Select/order sections
  # TODO: warn about sections that get lost here!
  result <- result[intersect(get_section_names_for_input(), names(result))]
  
  # assign class attribute
  set_class(result, "inp")
}

# get_section_names_for_input --------------------------------------------------
get_section_names_for_input <- function()
{
  # Read section names from "sections.csv"  
  info <- swmmr:::section_info()
  info <- info[!is.na(info$input), ]
  
  # Order section names by number in column "input" first
  # and by the section name second
  info$section[order(info$input, info$section)]
}
