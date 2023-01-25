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
  path_shp <- create_dir_if_required(file.path(path_out, "shp"))

  # dleutnant: 
  # Maybe instead of writing each section individually, we might use something 
  # like:
  # inp_to_sf(x) %>% 
  #   purrr::iwalk(., ~ sf::st_write(.x, file.path(
  #     path_out, paste0("shp/", .y, "_.shp"))
  #   ))
  
  # Configure the creation of shape files with a list:
  # - element names = names of inp sections for which to create shape files
  # - each element is a list with two elements:
  #   - "shape_name": base name of the shape file (without extension .shp)
  #   - "converter": function to be used to create the shape file
  config <- list(
    subcatchments = list(
      shape_name = "polygon",
      converter = subcatchments_to_sf
    ),
    conduits = list(
      shape_name = "link",
      converter = links_to_sf
    ),
    junctions = list(
      shape_name = "point", 
      converter = junctions_to_sf
    ),
    outfalls = list(
      shape_name = "outfall", 
      converter = outfalls_to_sf
    ),
    weirs = list(
      shape_name = "weir", 
      converter = weirs_to_sf
    ),
    orifices = list(
      shape_name = "orifices", 
      converter = orifices_to_sf
    ),
    pumps = list(
      shape_name = "pumps", 
      converter = pumps_to_sf
    ),
    storage = list(
      shape_name = "storages", 
      converter = storages_to_sf
    )
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

    # Convert section to sf if contained in x
    suppressMessages(sf::st_write(
      section_config$converter(x), 
      dsn = file.path(path_shp, sprintf(
        "%s_%s.shp", name, section_config$shape_name
      )), 
      delete_dsn = delete_dsn,
      quiet = quiet
    ))
  }
  
  msg(sprintf(
    "%d *.shp files were written to %s", length(section_names), path_shp
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
  
  # Check if txt folder exists in path_out otherwise create new directory
  create_dir_if_required(file.path(path_out, "txt"))
  
  # Check sections and add sections in new format to list options_txt
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
  
  # Save all options to a text file
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
  # If implemented: convert curves to txt files
  
  # Helper function
  msg <- function(...) if (!quiet) message(...)
  
  # Check class
  stopifnot(inherits(x, "inp"))
  
  # Check required elements
  if (!"curves" %in% names(x)) {
    msg("section curves is missing")
    return()
  }  
  
  # Check if txt folder exists in path_out otherwise create new directory
  path_txt <- create_dir_if_required(file.path(path_out, "txt"))
  
  # Replace NA with the most recent non-NA prior it
  x$curves <- zoo::na.locf(x$curves)
  
  # Split by curve name
  list_of_curves <- split(x$curves, x$curves$Name)
  
  curve_names <- unlist(lapply(
    lapply(list_of_curves, "[[", 1L), 
    "[[", 1L
  ))
  
  # Write table for each curve
  mapply(
    FUN = utils::write.table, 
    list_of_curves, 
    file = file.path(path_txt, sprintf("%s_%s.txt", name, curve_names)), 
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
  # If implemented: convert timeseries to dat files
  
  # Helper function
  msg <- function(...) if (!quiet) message(...)
  
  # Check class
  stopifnot(inherits(x, "inp"))

  # Check required elements
  if (!"timeseries" %in% names(x)) {
    msg("section timeseries is missing")
    return()
  }
  
  # Check if txt folder exists in path_out, otherwise create new directory
  path_dat <- create_dir_if_required(file.path(path_out, "dat"))
  
  # Convert section timeseries to swmm timeseries *.dat format
  
  # Separate timeseries
  series_names <- x$timeseries$Name
  starts <- which(!duplicated(series_names))
  
  timeseries <- list(
    start = starts,
    end = c(starts - 1L, length(series_names))[-1L],
    name = series_names[starts]
  )
  
  # One *.dat file per timeseries
  none_has_date <- all(is.na(x$timeseries$Date))
  all_have_date <- !anyNA(x$timeseries$Date)
  
  if (none_has_date || all_have_date) {
    
    columns <- c(if (all_have_date) "Date", "Time", "Value")
    
    mapply(
      FUN = function(start, end, ts) utils::write.table(
        x$timeseries[start:end, columns], 
        file.path(path_dat, sprintf("%s_timeseries_%s.dat", name, ts)),
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
  # Check class
  stopifnot(inherits(x, "inp"))
  
  # Check name
  if (is.null(name)) {
    clean_stop("name is missing")
  }
  
  # Check path_out
  if (is.null(path_out)) {
    clean_stop("path_out is missing")
  }
  
  # Convert and save input sections to shape files
  sections_to_shp(x, name, path_out, quiet = quiet)
  
  # Convert and save selection of input sections to txt files
  options_to_txt(x, name, path_out, quiet = quiet)
  
  # Write curves to txt
  curves_to_txt(x, name, path_out, quiet = quiet)
  
  # Convert timeseries to txt
  timeseries_to_dat(x, name, path_out, quiet = quiet)
}
