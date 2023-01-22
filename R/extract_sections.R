# extract_sections -------------------------------------------------------------
extract_sections <- function(x, trim = NULL)
{
  stopifnot(is.character(x))
  
  # Find row ranges of sections
  starts <- grep("\\[", x)
  ends <- c(starts[-1L] - 1L, length(x))
  
  # Get section names
  section_names <- gsub("\\[|\\]", "", x[starts])

  # Cut the lines between the section headers in [brackets]
  sections <- seq_along(starts) %>%
    lapply(function(i) x[seq.int(starts[i] + 1L, ends[i])]) %>%
    stats::setNames(section_names)
  
  if (is.null(trim)) {
    return(sections)
  }

  lapply(sections, trim_vector, trim = trim)
}
