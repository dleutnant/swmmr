#' import helper
#' @keywords internal
compare_to_dictionary <- function(shp = NULL, sf = NULL)
{
  if (!is.null(shp)) {
    return(rename_columns_using_dict(df = shp, from = "shp_abb"))
  }
  
  if (!is.null(sf)) {
    return(rename_columns_using_dict(df = sf, from = "org_swmm"))
  }
}

# rename_columns_using_dict ----------------------------------------------------
rename_columns_using_dict <- function(df, from, to = "int_shp_to_inp")
{
  # complete dictionary with all column names given in swmmr:
  d <- get_column_dictionary()
  
  # cut dictionary to column names 
  # - abbreviated and internal in shp_to_inp() or
  # - original and internal in sf_to_inp()
  # that differ:
  
  d <- d[!duplicated(d[[from]]) & d[[from]] != d[[to]], ]
  
  # set internal column names for abbreviated ones in data frame (representing
  # shp-file or sf-object)
  stats::setNames(df, replace_values(names(df), d[[from]], d[[to]]))
}
