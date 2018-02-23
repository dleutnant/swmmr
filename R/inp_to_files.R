#' conversion helper
#' @keywords internal
sections_to_shp <- function(x, name, path_out) {
  # ... convert inp to shp and save shape files
  # ... if implemented: convert weirs (links), orifices (links), pumps (links) and storages (point) to shape files

  # check class
  stopifnot(inherits(x, "inp"))

  # ... check if shp folder exists in path_out otherwise create new directory
  if (!file.exists(file.path(path_out, "shp"))) {
    dir.create(file.path(path_out, "shp"))
  }

  # dleutnant: 
  # There is currently an issue in writing sf objects on OS X which
  # causes R to crash if the file to be written already exists.
  # s. https://github.com/r-spatial/sf/issues/649
  # This hack per default overwrites an existent file only on OS X 
  # it is assumend that SWMM users are less likely OS X users... ;-(
  if (.get_os() == "darwin") {
    warning("Data source is deleted before attempting to write.")
    delete_dsn <- TRUE 
  } else {
    delete_dsn <- FALSE
  }

  # dleutnant: 
  # maybe instead of writing each section individually, we might
  # use sth:
  # inp_to_sf(x) %>% 
  #   purrr::iwalk(., ~ sf::st_write(.x, file.path(path_out, 
  #                                                paste0("shp/", .y, "_.shp"))))
  
  # ... convert sections to sf
  if ("subcatchments" %in% names(x)) {
    polygon <- subcatchments_to_sf(x)
    suppressMessages(sf::st_write(polygon,
                                  file.path(path_out, 
                                            paste0("shp/", name, "_polygon.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section subcatchments is missing")
  }

  if ("conduits" %in% names(x)) {
    link <- links_to_sf(x)
    suppressMessages(sf::st_write(link, 
                                  file.path(path_out, 
                                            paste0("shp/", name, "_link.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section conduits is missing")
  }

  if ("junctions" %in% names(x)) {
    point <- junctions_to_sf(x)
    suppressMessages(sf::st_write(point, file.path(path_out,
                                                   paste0("shp/", name, "_point.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section junctions is missing")
  }

  if ("outfalls" %in% names(x)) {
    outfall <- outfalls_to_sf(x)
    suppressMessages(sf::st_write(outfall, file.path(path_out, 
                                                     paste0("shp/", name, "_outfall.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section outfalls is missing")
  }

  if ("weirs" %in% names(x)) {
    weirs <- weirs_to_sf(x)
    suppressMessages(sf::st_write(weirs, file.path(path_out, 
                                                   paste0("shp/", name, "_weir.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section weirs is missing")
  }

  if ("orifices" %in% names(x)) {
    orifices <- orifices_to_sf(x)
    suppressMessages(sf::st_write(orifices, file.path(path_out, 
                                                      paste0("shp/", name, "_orifices.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section orifices is missing")
  }

  if ("pumps" %in% names(x)) {
    pumps <- pumps_to_sf(x)
    suppressMessages(sf::st_write(pumps, file.path(path_out, 
                                                   paste0("shp/", name, "_pumps.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section pumps is missing")
  }

  if ("storage" %in% names(x)) {
    storages <- storages_to_sf(x)
    suppressMessages(sf::st_write(storages, file.path(path_out, 
                                                      paste0("shp/", name, "_storages.shp")),
                                  delete_dsn = delete_dsn))
  } else {
    print("section storage is missing")
  }

  print(paste0("*.shp files were written to: ", path_out, "/shp"))
}

#' conversion helper
#' @keywords internal
options_to_txt <- function(x, name, path_out) {
  # convert section options, report, raingages, evaporation and if implemented: pollutant, landuse, buildup, washoff, coverage, (lid_controls lid_usage --> not in examples) to options.txt
  # check class and required elements
  stopifnot(inherits(x, "inp"))

  if ("options" %in% names(x)) {
    # ... check if txt folder exists in path_out otherwise create new directory
    if (!file.exists(file.path(path_out, "txt"))) {
      dir.create(file.path(path_out, "txt"))
    }

    # check sections and add sections in new format to list options_txt:

    options_txt <- list()

    if ("options" %in% names(x)) {
      options_txt[["[options]"]] <- x[["options"]] %>%
        apply(., 1, paste, collapse = "\t") %>%
        c("[options]", .)
    }
    if ("report" %in% names(x)) {
      options_txt[["[report]"]] <- x[["report"]] %>%
        apply(., 1, paste, collapse = "\t") %>%
        c("[report]", .)
    }
    if ("raingages" %in% names(x)) {
      options_txt[["[raingages]"]] <- t(x[["raingages"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[raingages]", .) %>%
		gsub("TIMESERIES\t", "TIMESERIES ", .)
    }
    if ("evaporation" %in% names(x)) {
      options_txt[["[evaporation]"]] <- x[["evaporation"]] %>%
        apply(., 1, paste, collapse = "\t") %>%
        c("[evaporation]", .)
    }
    if ("pollutants" %in% names(x)) {
      options_txt[["pollutants"]] <- t(x[["pollutants"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[pollutants]", .) %>%
		gsub("NA", "", .)
    }
    if ("landuses" %in% names(x)) {
      options_txt[["landuses"]] <- t(x[["landuses"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[landuses]", .) %>%
		gsub("NA", "", .)
    }
    if ("coverages" %in% names(x)) {
      options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[coverages]", .) %>%
		gsub("NA", "", .)
    }
    if ("buildup" %in% names(x)) {
      options_txt[["buildup"]] <- t(x[["buildup"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[buildup]", .) %>%
		gsub("NA", "", .)
    }
    if ("washoff" %in% names(x)) {
      options_txt[["washoff"]] <- t(x[["washoff"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[washoff]", .) %>%
		gsub("NA", "", .)
    }
    if ("coverages" %in% names(x)) {
      options_txt[["coverages"]] <- t(x[["coverages"]]) %>%
        apply(., 1, paste, collapse = "\t") %>%
        paste(names(.), ., sep = "\t") %>%
        c("[coverages]", .) %>%
		gsub("NA", "", .)
    }

    # unlist and save txt file
    writeLines(unlist(options_txt), con = file.path(path_out, paste0("txt/", name, "_options.txt")))

    print(paste0("*.txt file was written to: ", path_out, "/txt"))
  } else {
    print("section options is missing")
  }
}


#' conversion helper
#' @keywords internal
curves_to_txt <- function(x, name, path_out) {
  # if implemented: convert curves to txt files
  # check class and required elements
  stopifnot(inherits(x, "inp"))

  if ("curves" %in% names(x)) {

    # ... check if txt folder exists in path_out otherwise create new directory
    if (!file.exists(file.path(path_out, "txt"))) {
      dir.create(file.path(path_out, "txt"))
    }

    # ...replace NA with the most recent non-NA prior it
    x$curves <- zoo::na.locf(x$curves)

    # add coulumn for grouping
    x$curves$group <- x$curves$Name

    # group and split tibble
    list_of_curves <- x$curves %>%
      dplyr::group_by(group) %>%
      tidyr::nest() %>%
      dplyr::select(data) %>%
      unlist(recursive = F)

    # write table for each curve
    mapply(utils::write.table, list_of_curves, 
           file = paste0(path_out, "/txt/", name, "_", 
                         unlist(lapply(list_of_curves, "[[", 1, 1)), ".txt"), 
           sep = " ", dec = ".", col.names = F, row.names = F, quote = F)


    print(paste0("curve.txt files were written to:", path_out, "/txt"))
  } else {
    print("section curves is missing")
  }
}

#' conversion helper
#' @keywords internal
timeseries_to_dat <- function(x, name, path_out) {
  # if implemented: convert timeseries to dat files
  
  # check class and required elements
  stopifnot(inherits(x, "inp"))
  
  if ("timeseries" %in% names(x)) {
    # ... check if txt folder exists in path_out otherwise create new directory
    if (!file.exists(file.path(path_out, "dat"))) {
      dir.create(file.path(path_out, "dat"))
    }
    
    # ... convert section timeseries to swmm timeseries *.dat format
      # seperate timeseries
      timeseries <- list(
        start = which(duplicated(x$timeseries$Name) == F),
        end = c(which(duplicated(x$timeseries$Name) == F) - 1, length(x$timeseries$Name))[-1],
        name = x$timeseries$Name[duplicated(x$timeseries$Name) == F]
      )
      
      # one *.dat file per timeseries
      if(all(is.na(x$timeseries$Date))) {
        # if no date is given:
        mapply(function(start, end, ts) utils::write.table(x$timeseries[start:end, c("Time", "Value")], file.path(path_out, "dat", paste0(name, "_timeseries_", ts, ".dat")), row.names = F, col.names = F, quote = F), start = timeseries$start, end = timeseries$end, ts = timeseries$name)
      }else{
        if(all(is.na(x$timeseries$Date) == F)){
          # if date is given:
          mapply(function(start, end, ts) utils::write.table(x$timeseries[start:end, c("Date","Time", "Value")], file.path(path_out, "dat", paste0(name, "_timeseries_", ts, ".dat")), row.names = F, col.names = F, quote = F), start = timeseries$start, end = timeseries$end, ts = timeseries$name)
        }
      }
      
    print(paste0("timeseries.dat files were written to: ", path_out, "/dat"))
    
  } else {
    print("section timeseries is missing")
  }
}

#' Convert SWMM's .inp to .shp and txt files
#'
#' @param x An object of class inp.
#' @param name Give a name for the current model, e.g. "Example1".
#' @param path_out  Writeable directory name where to save the converted files. Folders: dat, shp and txt will be created if not existent.
#' @return .dat, .shp and/or .txt files.
#' @rdname inp_to_files
#' @export
inp_to_files <- function(x, name, path_out) {
  # check class
  stopifnot(inherits(x, "inp"))

  # check name
  if (is.null(name)) {
    stop("name is missing")
  }

  # check path_out
  if (is.null(path_out)) {
    stop("path_out is missing")
  }

  # convert and save input sections to shape files
  sections_to_shp(x, name, path_out)

  # convert and save selection of input sections to txt files
  options_to_txt(x, name, path_out)

  # write curves to txt
  curves_to_txt(x, name, path_out)

  # timeseries to txt
  timeseries_to_dat(x, name, path_out)
}