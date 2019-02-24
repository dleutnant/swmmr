clean_stop <- function(...) {
  
  stop(..., call. = FALSE)
}

clean_warning <- function(...) {
  
  warning(..., call. = FALSE)
}

stop_on_bad_index <- function(index, choices) {

  # Possible values for index
  values <- seq_along(choices) - 1
  
  if (! is.numeric(index) || length(index) != 1L || ! index %in% values) {
    clean_stop(sprintf(
      "The index must be an integer value between 0 and %d:\n%s", 
      values[length(values)], 
      paste(sprintf("%4i: %s", values, choices), collapse = "\n")
    ))
  }
}
