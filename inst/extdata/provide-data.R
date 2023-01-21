`%>%` <- magrittr::`%>%`

column_defaults <- swmmr:::get_column_defaults()

defaults <- column_defaults %>%
  lapply(function(xx) {
    data.frame(
      column = names(xx), 
      value = as.character(xx),
      type = sapply(xx, mode), 
      stringsAsFactors = FALSE
    )
  }) %>%
  kwb.utils::rbindAll(nameColumn = "section", namesAsFactor = FALSE) %>%
  kwb.utils::moveColumnsToFront("section")

file <- "inst/extdata/defaults.csv"

write.csv(defaults, file, row.names = FALSE, quote = FALSE)

defaults2 <- read.csv(file)

identical(defaults, defaults2)

# Convert defaults back to list as returned by get_column_defaults()
tables <- split(defaults, kwb.utils::toFactor(defaults$section))

column_defaults2 <- lapply(tables, function(x) {
  #x <- tables[[1L]]
  stats::setNames(nm = x$column, lapply(seq_len(nrow(x)), function(i) {
    do.call(paste0("as.", x$type[i]), list(x$value[i]))
  }))
})

identical(column_defaults, column_defaults2)
