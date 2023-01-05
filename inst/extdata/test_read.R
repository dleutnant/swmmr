package_file <- function(...) system.file(..., package = "swmmr")

inp_file <- package_file("extdata", "Example1.inp")

exec <- "/home/hauke/CProgramming/Stormwater-Management-Model/bin/run-swmm"

out_file <- file.path(tempdir(), "swmm-Example1.out")
out_file <- "/home/hauke/Downloads/SWMM/result.out"

file.size(out_file) / (1024^3)

result <- swmmr:::OpenSwmmOutFile(out_file)
swmmr:::GetSwmmTimes()
swmmr:::CloseSwmmOutFile()

#kwb.utils::assignPackageObjects("swmmr")
#kwb.utils::assignArgumentDefaults(swmmr::read_out)
#file = out_file; byObject = FALSE; multiColumn = TRUE
system.time(result2 <- swmmr::read_out(
  file = out_file, byObject = FALSE, multiColumn = TRUE
  , iType = 0, object_name = "SV8", firstPeriod = 1, lastPeriod = 5*86400, 
  vIndex = 0
))
identical(result, result2)
#, firstPeriod = 1, lastPeriod = 100)

swmmr::run_swmm(inp_file, out = out_file, exec = exec)

#swmmr:::OpenSwmmOutFile(out_file)
#swmmr:::CloseSwmmOutFile()

#swmmr::read_out(out_file, iType = 2, object_name = "1", vIndex = 0)
args <- list(
  file = out_file, 
  iType = 0, 
  object_name = c("SV8", "SV27", "SV34"), 
  vIndex = c(0, 2, 4),
  multiColumn = TRUE, 
  byObject = FALSE
)

result <- do.call(swmmr::read_out, args)

do.call(swmmr::read_out, c(args, firstPeriod = 200))
do.call(swmmr::read_out, c(args, firstPeriod = -1))
do.call(swmmr::read_out, c(args, firstPeriod = 35, lastPeriod = 33))
do.call(swmmr::read_out, c(args, firstPeriod = 2, lastPeriod = 12))

swmmr:::GetSwmmResultPart(iType = 1, iIndex = 1, vIndex = 1, firstPeriod = 0, lastPeriod = 0)

#swmmr:::GetSwmmResultPart(iType = 2, iIndex = 1, vIndex = 0, firstPeriod = 1, lastPeriod = 1)