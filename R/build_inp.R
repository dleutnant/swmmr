#' build_inp
#'
#' Builds an SWMM inp file based on a template.
#'
#' @title build_inp
#' @param tmp Name and path to a template file.
#' @param inp Name and path to a new input file.
#' @param parameter Names of parameters to be modified.
#' @param values The new parameter values.
#' @param factor Is the new parameter value a factor?
#' @rdname build_inp
#' @export build_inp
build_inp <- function(tmp,
                      inp = NULL,
                      parameter,
                      values, 
                      factor) {
  
  if (length(parameter) != length(values) | length(values) != length(factor)) stop("vector lengths unequal.")
  
  template <- readLines(tmp)
  
  # for each parameter
  for (i in 1:length(parameter)) {
  
    lines_with_parameter <- grep(parameter[i], template)

    # find dummy position
    dummy_pos <- t(sapply(gregexpr("$", template[lines_with_parameter],
                                   fixed = T),
                          "["))
    
    # vars with dollar sign
    vars <- sapply(1:nrow(dummy_pos), 
                   function(x) substr(template[lines_with_parameter][x],
                                      start = dummy_pos[x,1], 
                                      stop = dummy_pos[x,2]))
    
    # extract values
    # remove dollar sign
    vars_ <- gsub(pattern = "\\$", replacement = "", x = vars)
    
    # modify values
    if (factor[i] == TRUE) {
      
      if (-1 %in% (sapply(gregexpr("_", 
                                   vars_, 
                                   fixed = T),
                          "["))) stop("parameter group error.")
      
      # split dummy var into parameter - value_old structure
      vars_split <- data.frame(Reduce(rbind, strsplit(vars_, split = "_")), 
                               stringsAsFactors = FALSE)
      colnames(vars_split) <- c("parameter","value_old")
      
      value_new <- as.character(as.numeric(vars_split$value_old) * as.numeric(values[i]))  
    
    } else {
      
      vars_split <- data.frame(parameter = vars_)
      value_new <- rep(as.character(values[i]), times = length(vars_))
      
    }

    for (j in 1:length(value_new)) {
      
      # get pattern to substitute
      pat <- grep(pattern = vars_split$parameter[j], 
                  x = vars, 
                  value = TRUE, 
                  fixed = TRUE)

      # substitute with value
      template <- gsub(pattern = pat, 
                       replacement = value_new[j], 
                       x = template,
                       fixed = TRUE)
          
    }

  }
  
  # write inp file to disk
  if (!is.null(inp)) {
    
    writeLines(text = template, con = inp)
    
  } else {
    
    invisible(template)
  }
  
}
