#' conversion helper
#' @keywords internal
sections_to_shp <- function(
    x, name, path_out, quiet = FALSE, delete_dsn = FALSE
)
{
  # Convert inp to shp and save shape files
  #
  # If implemented: convert the following objects to shape files
  # - weirs (links), 
  # - orifices (links), 
  # - pumps (links), and 
  # - storages (point) 
  
  # Check class
  stopifnot(inherits(x, "inp"))
  
  # Helper function
  msg <- function(...) if (!quiet) message(...)
  
  # Create new folder "shp" if it does not exist in path_out
  shape_dir <- file.path(path_out, "shp")
  create_dir_if_required(shape_dir)

  # dleutnant: 
  # Maybe instead of writing each section individually, we might use something 
  # like:
  # inp_to_sf(x) %>% 
  #   purrr::iwalk(., ~ sf::st_write(.x, file.path(
  #     path_out, paste0("shp/", .y, "_.shp"))
  #   ))
  
  # Configuration with:
  # - element names = section names
  # - each element is a list with one named element:
  #   - name = name of the shape file
  #   - value = function that converts from section to shape
  config <- list(
    subcatchments = list(polygon = subcatchments_to_sf),
    conduits = list(link = links_to_sf),
    junctions = list(point = junctions_to_sf),
    outfalls = list(outfall = outfalls_to_sf),
    weirs = list(weir = weirs_to_sf),
    orifices = list(orifices = orifices_to_sf),
    pumps = list(pumps = pumps_to_sf),
    storage = list(storages = storages_to_sf)
  )

  section_names <- names(config)
  is_missing <- !(section_names %in% names(x))
  
  if (any(is_missing)) {
    msg(sprintf(
      "Section '%s' is missing in the given input.", 
      section_names[is_missing]
    ))
    section_names <- section_names[!is_missing]
  }
  
  # Loop through the sections for which to write shape files
  for (section_name in section_names) {
    
    section_config <- config[[section_name]]
    converter <- section_config[[1L]]
    shape_name <- names(section_config)[1L]

    # Convert section to sf if contained in x
    suppressMessages(sf::st_write(
      converter(x), 
      dsn = file.path(shape_dir, sprintf("%s_%s.shp", name, shape_name)), 
      delete_dsn = delete_dsn,
      quiet = quiet
    ))
  }
  
  msg(sprintf(
    "%d *.shp files were written to %s", length(shape_names), shape_dir
  ))
}

#' conversion helper
#' @keywords internal
options_to_txt <- function(x, name, path_out, quiet = FALSE)
{
  # convert section options, report, raingages, evaporation and if implemented:
  # pollutant, landuse, buildup, washoff, coverage, (lid_controls lid_usage -->
  # not in examples) to options.txt
  
  # check class and required elements
  stopifnot(inherits(x, "inp"))
  
  # helper functions
  msg <- function(...) if (!quiet) message(...)
  tab_collapse_rows <- function(x) apply(x, 1L, paste, collapse = "\t")
  tab_prepend_names <- function(x) paste(names(x), x, sep = "\t")
  add_section_name <- function(x, name) c(in_brackets(name), x)
  remove_na <- function(x) gsub("NA", "", x)
  
  if (!"options" %in% names(x)) {
    msg("section options is missing")
    return()
  }
  
  # ... check if txt folder exists in path_out otherwise create new directory
  create_dir_if_required(file.path(path_out, "txt"))
  
  # check sections and add sections in new format to list options_txt:
  
  options_txt <- list()
  
  if ("options" %in% names(x)) {
    options_txt[["[options]"]] <- x[["options"]] %>%
      tab_collapse_rows() %>%
      add_section_name("options")
  }
  
  if ("report" %in% names(x)) {
    options_txt[["[report]"]] <- x[["report"]] %>%
      tab_collapse_rows() %>%
      add_section_name("report")
  }
  
  if ("raingages" %in% names(x)) {
    options_txt[["[raingages]"]] <- t(x[["raingages"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("raingages") %>%
      gsub("TIMESERIES\t", "TIMESERIES ", .)
  }
  
  if ("evaporation" %in% names(x)) {
    options_txt[["[evaporation]"]] <- x[["evaporation"]] %>%
      tab_collapse_rows() %>%
      add_section_name("evaporation")
  }
  
  if ("pollutants" %in% names(x)) {
    options_txt[["pollutants"]] <- t(x[["pollutants"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("pollutants") %>%
      remove_na()
  }
  
  if ("landuses" %in% names(x)) {
    options_txt[["landuses"]] <- t(x[["landuses"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("landuses") %>%
      remove_na()
  }
  
  if ("coverages" %in% names(x)) {
    options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("coverages") %>%
      remove_na()
  }
  
  if ("buildup" %in% names(x)) {
    options_txt[["buildup"]] <- t(x[["buildup"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("buildup") %>%
      remove_na()
  }
  
  if ("washoff" %in% names(x)) {
    options_txt[["washoff"]] <- t(x[["washoff"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("washoff") %>%
      remove_na()
  }
  
  if ("coverages" %in% names(x)) {
    options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
      tab_collapse_rows() %>%
      tab_prepend_names() %>%
      add_section_name("coverages") %>%
      remove_na()
  }
  
  # unlist and save txt file
  writeLines(
    unlist(options_txt), 
    con = file.path(path_out, "txt", paste0(name, "_options.txt"))
  )
  
  msg(sprintf("*.txt file was written to %s/txt", path_out))
}

#' conversion helper
#' @keywords internal
curves_to_txt <- function(x, name, path_out, quiet = FALSE)
{
  # if implemented: convert curves to txt files
  # check class and required elements
  stopifnot(inherits(x, "inp"))
  
  # helper function
  msg <- function(...) if (!quiet) message(...)
  
  if (!"curves" %in% names(x)) {
    msg("section curves is missing")
    return()
  }  
  
  # ... check if txt folder exists in path_out otherwise create new directory
  create_dir_if_required(file.path(path_out, "txt"))
  
  # ...replace NA with the most recent non-NA prior it
  x$curves <- zoo::na.locf(x$curves)
  
  # ... split by curve name
  list_of_curves <- split(x$curves, x$curves$Name)
  
  # write table for each curve
  mapply(
    FUN = utils::write.table, 
    list_of_curves, 
    file = file.path(path_out, "txt", sprintf(
      "%s_%s.txt", 
      name, 
      unlist(lapply(lapply(list_of_curves, "[[", 1), "[[", 1))
    )), 
    sep = " ", 
    dec = ".", 
    col.names = FALSE, 
    row.names = FALSE, 
    quote = FALSE
  )
  
  msg(sprintf("curve.txt files were written to %s/txt", path_out))
}

#' conversion helper
#' @keywords internal
timeseries_to_dat <- function(x, name, path_out, quiet = FALSE)
{
  # if implemented: convert timeseries to dat files
  
  # check class and required elements
  stopifnot(inherits(x, "inp"))
  
  # helper function
  msg <- function(...) if (!quiet) message(...)
  
  if (!"timeseries" %in% names(x)) {
    msg("section timeseries is missing")
    return()
  }
  
  # ... check if txt folder exists in path_out otherwise create new directory
  create_dir_if_required(file.path(path_out, "dat"))
  
  # ... convert section timeseries to swmm timeseries *.dat format
  # seperate timeseries
  series_names <- x$timeseries$Name
  starts <- which(!duplicated(series_names))
  
  timeseries <- list(
    start = starts,
    end = c(starts - 1L, length(series_names))[-1L],
    name = series_names[starts]
  )
  
  # one *.dat file per timeseries
  none_has_date <- all(is.na(x$timeseries$Date))
  all_have_date <- !anyNA(x$timeseries$Date)
  
  if (none_has_date || all_have_date) {
    
    columns <- c(if (all_have_date) "Date", "Time", "Value")
    
    mapply(
      FUN = function(start, end, ts) utils::write.table(
        x$timeseries[start:end, columns], 
        file.path(path_out, "dat", paste0(name, "_timeseries_", ts, ".dat")),
        row.names = FALSE, 
        col.names = FALSE, 
        quote = FALSE
      ),
      start = timeseries$start, 
      end = timeseries$end, 
      ts = timeseries$name
    )
    
    msg(sprintf("timeseries.dat files were written to %s/dat", path_out))
  }
}

#' Convert SWMM's .inp to .shp and txt files
#'
#' @param x An object of class inp.
#' @param name Give a name for the current model, e.g. "Example1".
#' @param path_out  Writeable directory name where to save the converted files.
#' Folders: dat, shp and txt will be created if not existent. Default is the 
#' current working directory of the R process.
#' @param quiet if \code{TRUE} debug messages are suppressed, default: 
#' \code{FALSE}
#' @return .dat, .shp and/or .txt files.
#' @rdname inp_to_files
#' @export
inp_to_files <- function(x, name, path_out = getwd(), quiet = FALSE)
{
  # check class
  stopifnot(inherits(x, "inp"))
  
  # check name
  if (is.null(name)) {
    clean_stop("name is missing")
  }
  
  # check path_out
  if (is.null(path_out)) {
    clean_stop("path_out is missing")
  }
  
  # convert and save input sections to shape files
  sections_to_shp(x, name, path_out, quiet = quiet)
  
  # convert and save selection of input sections to txt files
  options_to_txt(x, name, path_out, quiet = quiet)
  
  # write curves to txt
  curves_to_txt(x, name, path_out, quiet = quiet)
  
  # timeseries to txt
  timeseries_to_dat(x, name, path_out, quiet = quiet)
}
