#
# Source the whole script first, then interactively go through the MAIN sections
#

# MAIN 1: Provide column default data in "defaults.csv" ------------------------
if (FALSE)
{
  column_defaults <- swmmr:::get_column_defaults()
  
  defaults <- column_defaults %>%
    lapply(function(x) {
      data.frame(
        column = names(x), 
        value = as.character(x),
        type = sapply(x, mode), 
        stringsAsFactors = FALSE
      )
    }) %>%
    kwb.utils::rbindAll(nameColumn = "section", namesAsFactor = FALSE) %>%
    kwb.utils::moveColumnsToFront("section")
  
  file <- "inst/extdata/config/defaults.csv"
  
  write_csv(defaults, file)

  # Check that we can reproduce the original data
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
}

# MAIN 2: Provide section data in "sections.csv" -------------------------------
if (FALSE)
{
  writeLines(con = "inst/extdata/config/sections.csv", c(
    paste0("input,", swmmr:::input_sections),
    paste0("report,", swmmr:::report_sections)
  ))
}

# MAIN 3: Provide column data in "columns.csv" ---------------------------------
if (FALSE)
{
  column_info <- swmmr:::section_columns()
  
  lapply(column_info, function(x) data.frame(column = x)) %>%
    dplyr::bind_rows(.id = "section") %>%
    write_csv("inst/extdata/config/columns.csv")
}

# MAIN 4: Provide erroneous error files ----------------------------------------
if (FALSE)
{
  # For test purposes, create some basic report files and store them in 
  # inst/extdata/testdata
  
  files_inp <- tempfile(paste0("ex", 1:3), fileext = ".rpt")
  
  swmmr:::create_dir_if_required("inst/extdata/testdata")
  
  inp_files <- file.path(getwd(), sprintf(
    "inst/extdata/testdata/error-%d.inp", 1:3
  ))
  
  writeLines("", inp_files[1L])
  writeLines("nonsense", inp_files[2L])
  writeLines("[nonsense]", inp_files[3L])
  
  swmmr::run_swmm(inp_files[1L])
  swmmr::run_swmm(inp_files[2L])
  swmmr::run_swmm(inp_files[3L])
  
  cat(text_no_output_file(
    path_out = "a", 
    path_rpt = "inst/extdata/testdata/error-1.rpt"
  ))
}

# MAIN 5: Merge data-model related tables --------------------------------------
if (FALSE)
{
  library(magrittr)
  
  files <- dir(swmmr:::system_file("extdata/config"))
  
  records <- lapply(files, function(origin) {
    swmmr:::read_data_model(origin) %>%
      cbind(stats::setNames(data.frame(x = "x"), swmmr:::remove_extension(origin)))
  })
  
  names(records) <- files
  
  sapply(records, nrow)
  sapply(records, names)
  
  records$dictionary.csv$section <- swmmr:::replace_values(
    records$dictionary.csv$section, 
    from = c("subcatchment", "subarea", "infiltration_Horton", "junction", 
             "conduit", "xsection", "pump", "weir", "storage"),
    to = c("subcatchments", "subareas", "infiltration_horton", "junctions", 
           "conduits", "xsections", "pumps", "weirs", "storages") 
  )
  
  records$sections.csv$section
  
  merged <- merge(
    records$columns.csv, 
    records$defaults.csv,
    by = c("section", "column"),
    all = TRUE
  ) %>%
    merge(
      records$dictionary.csv, 
      by.x = c("section", "column"),
      by.y = c("section", "org_swmm"),
      all = TRUE
    ) %>%
    kwb.utils::moveColumnsToFront(c(
      "section", "column", "columns", "defaults", "dictionary"
    ))
  
  nrow(merged)
  # 511 -> 502 -> 495 -> 491 -> 485 -> 478 -> 471 -> 464
  
  View(merged)
}

# Define functions -------------------------------------------------------------
`%>%` <- magrittr::`%>%`

write_csv <- function(x, file) {
  write.csv(x, file, row.names = FALSE, quote = FALSE)
}
