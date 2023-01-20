#' conversion helper
#' @keywords internal
sections_to_shp <- function(x, name, path_out, quiet = FALSE)
{
  # ... convert inp to shp and save shape files
  # ... if implemented: convert weirs (links), orifices (links), pumps (links)
  #     and storages (point) to shape files
  
  # check class
  stopifnot(inherits(x, "inp"))
  
  # helper function
  msg <- function(...) if (!quiet) message(...)
  
  # ... check if shp folder exists in path_out otherwise create new directory
  shape_folder <- file.path(path_out, "shp")
  
  if (!file.exists(shape_folder)) {
    dir.create(shape_folder)
  }
  
  # dleutnant: 
  # There is currently an issue in writing sf objects on OS X which
  # causes R to crash if the file to be written already exists.
  # s. https://github.com/r-spatial/sf/issues/649
  # This hack per default overwrites an existent file only on OS X 
  # it is assumend that SWMM users are less likely OS X users... ;-(
  
  # update: 180413 issue is fixed in sf 0.6-1
  # if (.get_os() == "darwin") {
  #   warning("Data source is deleted before attempting to write.")
  #   delete_dsn <- TRUE 
  # } else {
  #   delete_dsn <- FALSE
  # }
  delete_dsn <- FALSE
  
  # dleutnant: 
  # maybe instead of writing each section individually, we might use sth:
  # inp_to_sf(x) %>% 
  #   purrr::iwalk(., ~ sf::st_write(.x, file.path(path_out, paste0("shp/", .y, "_.shp"))))
  
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
  
  shape_dir <- file.path(path_out, "shp")
  
  for (section in names(config)) {
    
    section_config <- config[[section]]
    conversion_function <- section_config[[1L]]
    shape_name <- names(section_config)[1L]
    
    # ... convert section to sf if contained in x
    if (section %in% names(x)) {
      
      suppressMessages(sf::st_write(
        conversion_function(x), 
        dsn = file.path(shape_dir, paste0(name, "_", shape_name, ".shp")), 
        delete_dsn = delete_dsn,
        quiet = quiet
      ))
      
    } else {
      
      msg(sprintf("section %s is missing", section))
    }
  }
  
  msg(sprintf("*.shp files were written to %s", shape_dir))
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
  tab_collapsed_rows <- function(x) apply(x, 1L, paste, collapse = "\t")
  tab_prepend_names <- function(x) paste(names(x), x, sep = "\t")
  add_section_name <- function(x, name) c(in_brackets(name), x)
  remove_na <- function(x) gsub("NA", "", x)
  
  if ("options" %in% names(x)) {
    
    # ... check if txt folder exists in path_out otherwise create new directory
    create_dir_if_required(file.path(path_out, "txt"))
    
    # check sections and add sections in new format to list options_txt:
    
    options_txt <- list()
    
    if ("options" %in% names(x)) {
      options_txt[["[options]"]] <- x[["options"]] %>%
        tab_collapsed_rows() %>%
        add_section_name("options")
    }
    
    if ("report" %in% names(x)) {
      options_txt[["[report]"]] <- x[["report"]] %>%
        tab_collapsed_rows() %>%
        add_section_name("report")
    }
    
    if ("raingages" %in% names(x)) {
      options_txt[["[raingages]"]] <- t(x[["raingages"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("raingages") %>%
        gsub("TIMESERIES\t", "TIMESERIES ", .)
    }
    
    if ("evaporation" %in% names(x)) {
      options_txt[["[evaporation]"]] <- x[["evaporation"]] %>%
        tab_collapsed_rows() %>%
        add_section_name("evaporation")
    }
    
    if ("pollutants" %in% names(x)) {
      options_txt[["pollutants"]] <- t(x[["pollutants"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("pollutants") %>%
        remove_na()
    }
    
    if ("landuses" %in% names(x)) {
      options_txt[["landuses"]] <- t(x[["landuses"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("landuses") %>%
        remove_na()
    }
    
    if ("coverages" %in% names(x)) {
      options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("coverages") %>%
        remove_na()
    }
    
    if ("buildup" %in% names(x)) {
      options_txt[["buildup"]] <- t(x[["buildup"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("buildup") %>%
        remove_na()
    }
    
    if ("washoff" %in% names(x)) {
      options_txt[["washoff"]] <- t(x[["washoff"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("washoff") %>%
        remove_na()
    }
    
    if ("coverages" %in% names(x)) {
      options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
        tab_collapsed_rows() %>%
        tab_prepend_names() %>%
        add_section_name("coverages") %>%
        remove_na()
    }
    
    # unlist and save txt file
    writeLines(
      unlist(options_txt), 
      con = file.path(path_out, paste0("txt/", name, "_options.txt"))
    )
    
    msg(sprintf("*.txt file was written to %s/txt", path_out))
    
  } else {
    
    msg("section options is missing")
  }
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
  
  if ("curves" %in% names(x)) {
    
    # ... check if txt folder exists in path_out otherwise create new directory
    if (!file.exists(file.path(path_out, "txt"))) {
      dir.create(file.path(path_out, "txt"))
    }
    
    # ...replace NA with the most recent non-NA prior it
    x$curves <- zoo::na.locf(x$curves)
    
    # ... split by curve name
    list_of_curves <- split(x$curves, x$curves$Name)
    
    # write table for each curve
    mapply(utils::write.table, list_of_curves, 
           file = paste0(path_out, "/txt/", name, "_", 
                         unlist(lapply(lapply(list_of_curves, "[[", 1), "[[", 1)), ".txt"), 
           sep = " ", dec = ".", col.names = F, row.names = F, quote = F)
    
    msg(sprintf("curve.txt files were written to %s/txt", path_out))
    
  } else {
    
    msg("section curves is missing")
  }
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
