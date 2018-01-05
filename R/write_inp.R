#' Write SWMM's .inp file
#'
#' Writes an `inp` object to disk which can be read and run by SWMM.
#' 
#' @param x An object of class 'inp', created by \code{\link{read_inp}}.
#' @inheritParams utils::write.table
#' @examples  
#' \dontrun{
#'   write_inp(inp, "~/model.inp")
#' } 
#' @rdname write_inp
#' @export 
write_inp <- function(x, file) {
  
  # check class and required elements
  stopifnot(inherits(x, "inp"))
  
  # use sink to write sections to
  sink(file = file)
  
  # for each setion ...
  for (section in names(x)) {
    
    # write section name
    cat(paste0("[", toupper(section), "]"), "\n")  
    
    # write the data without names 
    utils::write.table(x = x[section],
                       quote = FALSE,
                       na = "",
                       row.names = FALSE, 
                       col.names = FALSE)
    
    # write newline to separate sections
    cat("\n")
    
  }
  
  # close sink
  sink()
  
}
