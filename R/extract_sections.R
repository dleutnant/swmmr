# extract_sections -------------------------------------------------------------
extract_sections <- function(x, trim = NULL)
{
  stopifnot(is.character(x))
  
  # Find row ranges of sections
  starts <- grep("\\[", x)
  ends <- c(starts[-1L] - 1L, length(x))
  
  # Get section names
  section_names <- x[starts] %>%
    gsub(pattern = "\\[|\\]", replacement = "")
  
  sections <- lapply(seq_along(starts), function(i) {
    x[seq.int(starts[i] + 1L, ends[i])]
  })

  if (!is.null(trim)) {
    sections <- lapply(sections, trim_vector, trim = trim)
  }
  
  stats::setNames(sections, section_names)
}
