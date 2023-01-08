# example_input_files ----------------------------------------------------------
example_input_files <- function()
{
  system.file(
    "extdata", 
    paste0("Example", 1:6, ".inp"), 
    package = "swmmr", 
    mustWork = TRUE
  )
}
