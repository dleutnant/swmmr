# inspired by http://r-pkgs.had.co.nz/r.html
.onLoad <- function(libname, pkgname)
{
  # load options available
  op <- options()
  
  # get path to swmm executable
  swmmr.exec <- .get_exec()
  
  # set option
  op.swmmr <- list(
    swmmr.exec = swmmr.exec
  )
  
  # get the difference to available options
  toset <- !(names(op.swmmr) %in% names(op))
  
  # save options
  if (any(toset)) options(op.swmmr[toset])
  
  invisible()
}

# input section
#' @keywords internal
input_sections <- c(
  "aquifers",
  "backdrop",
  "buildup",
  "conduits", 
  "controls", 
  "coordinates",
  "coverages",
  "curves",
  "dividers",
  "dwf",
  "evaporation",
  "events",
  "files",
  "groundwater",
  "hydrographs",
  "iiflows",
  "infiltration",
  "inflows",
  "junctions",
  "labels",
  "landuses",
  "lid_controls",
  "lid_usage",
  "loadings",
  "losses",
  "map",
  "options", 
  "orifices",
  "outfalls",
  "outlets",
  "patterns",
  "pollutants",
  "polygons",
  "profiles",
  "pumps",
  "raingages",
  "report",
  "snowpacks",
  "storage",
  "subareas",
  "subcatchments",
  "symbols",
  "tags",
  "temperature",
  "timeseries",
  "title",
  "treatment",
  "vertices",
  "washoff",
  "weirs",
  "xsections"
)

# report section
#' @keywords internal
report_sections <- c(
  "Element Count",
  "Pollutant Summary",
  "Landuse Summary",
  "Raingage Summary",
  "Subcatchment Summary",
  "Node Summary",
  "Link Summary",
  "Cross Section Summary",
  "Analysis Options",
  #"Rainfall File Summary",
  #"Rainfall Dependent I/I",
  #"Control Actions Taken",
  "Runoff Quantity Continuity", 
  "Runoff Quality Continuity",
  "Groundwater Continuity",
  "Flow Routing Continuity",
  "Quality Routing Continuity",
  "Highest Continuity Errors",
  "Time-Step Critical Elements",
  "Highest Flow Instability Indexes", 
  "Routing Time Step Summary", 
  #"Subcatchment Results",
  "Subcatchment Runoff Summary",
  "LID Performance Summary",
  "Subcatchment Washoff Summary",
  #"Node Results",
  "Node Depth Summary", 
  "Node Inflow Summary", 
  "Node Flooding Summary", 
  "Outfall Loading Summary", 
  #"Link Results",
  "Link Flow Summary", 
  "Conduit Surcharge Summary",
  "Link Pollutant Load Summary",
  "Pumping Summary",
  "Groundwater Summary", # example? 
  "LID Control Summary",
  "Node Surcharge Summary",
  "Storage Volume Summary",
  "Flow Classification Summary"
)

section_info <- read.csv(system.file(
  "extdata/sections.csv", package = "swmmr"
))

stopifnot(identical(
  report_sections,
  section_info$section[section_info$type == "report"]
))

stopifnot(identical(
  input_sections,
  section_info$section[section_info$type == "input"]
))
