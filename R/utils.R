clean_stop <- function(...) {
  
  stop(..., call. = FALSE)
}

clean_warning <- function(...) {
  
  warning(..., call. = FALSE)
}
