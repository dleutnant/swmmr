#' Read SWMM's LID Report File
#'
#' Reads a SWMM's LID Report File and returns a tibble
#' 
#' @param x Name (incl. path) to a LID report file.
#' @return A tibble
#' @param ... optional arguments passed to \code{\link[readr]{read_table2}}
#' @examples  
#' \dontrun{
#' tbl_lid_rpt <- read_lid_rpt("lid_rpt.txt")
#' } 
#' @rdname read_lid_rpt
#' @export
read_lid_rpt <- function(x, ...) { 
  
  # set header
  header <- c("Date", "Time",
              "Elapsed Time", "Total Inflow", "Total Evap", "Surface Infil", 
              "Pavement Perc", "Soil Perc", "Storage Exfil", "Surface Runoff",
              "Drain OutFlow", "Surface Level", "Pavement Level",
              "Soil Moisture", "Storage Level")
  
  # parse project and LID Unit
  meta_info <- readr::read_lines(file = x, skip = 2, n_max = 2) %>%
    purrr::map_chr( ~ substr(., 10, nchar(.))) %>% 
    trimws()
  
  # get the data
  tbl_lid_rpt <- readr::read_table2(file = x,  
                                    col_names = header, 
                                    col_types = "ccddddddddddddd",
                                    skip = 9, 
                                    ...) %>% 
    # make one datetime col
    tidyr::unite(col = "DateTime", Date, Time, sep = " ") %>% 
    dplyr::mutate(DateTime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"), 
                  Project = meta_info[1], 
                  `LID Unit` = meta_info[2])
  
  return(tbl_lid_rpt)
  
}