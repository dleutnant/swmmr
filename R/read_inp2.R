# types
input_section_type <- c(
  "TITLE",        "OPTION",       "FILE",         "RAINGAGE",
  "TEMP",         "EVAP",         "SUBCATCH",     "SUBAREA",
  "INFIL",        "AQUIFER",      "GROUNDWATER",  "SNOWMELT",
  "JUNCTION",     "OUTFALL",      "STORAGE",      "DIVIDER",
  "CONDUIT",      "PUMP",         "ORIFICE",      "WEIR",
  "OUTLET",       "XSECTION",     "TRANSECT",     "LOSSES",
  "CONTROL",      "POLLUTANT",    "LANDUSE",      "BUILDUP",
  "WASHOFF",      "COVERAGE",     "INFLOW",       "DWF",
  "PATTERN",      "RDII",         "UNITHYD",      "LOADING",
  "TREATMENT",    "CURVE",        "TIMESERIES",   "REPORT",
  "COORDINATE",   "VERTICES",     "POLYGON",      "LABEL",
  "SYMBOL",       "BACKDROP",     "TAG",          "PROFILE",
  "MAP",          "LID_CONTROL",  "LID_USAGE",    "GWF",                   
  "ADJUST",       "EVENT"
  )

read_inp2 <- function(x) {
  
  x <- "../../GitLab/models/residential/qm_rc_tmpl.inp"
  
  # read lines
  inp_lines <- readLines(x)
  
  # find section lines
  section_lines <- grep("\\[", inp_lines, value = F)
  
  # helper var
  n_sections <- length(section_lines)
  
  # get section names
  section_types <- gsub(pattern = "[[:punct:]]", 
                        replacement = "",  
                        x = inp_lines[section_lines])
  
  # get last line per section
  section_length <- c(section_lines[-1]-1,length(inp_lines)) 
  
  # prepare result list
  list_of_sections <- vector(mode = "list", length = n_sections)
  
  # create list with sections  
  list_of_sections <- purrr::map2(section_lines, section_length, ~ inp_lines[(.x + 1):.y]) %>% 
    purrr::set_names(base::tolower(section_types))
  
  # sec has  divider line?
  sec_has_div <- purrr::map(list_of_sections, ~ grepl(pattern = "-----", .)) %>% 
    purrr::map_lgl(any)
  
  
  ## TODO and handle  different types of sections
  # tmp <- purrr::quietly(purrr::map(list_of_sections, ~ utils::read.table(text = .),
  #                   sep = "", 
  #                   dec = ".", 
  #                   numerals = "no.loss",
  #                   stringsAsFactors = FALSE, 
  #                   fill = TRUE))
  # 
  
  
    
}