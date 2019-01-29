package_file <- function(...) system.file(..., package = "swmmr")

inp_file <- package_file("extdata", "Example1.inp")

exec <- "/home/hauke/CProgramming/Stormwater-Management-Model/bin/run-swmm"

out_file <- file.path(tempdir(), "swmm-Example1.out")

swmmr::run_swmm(inp_file, out = out_file, exec = exec)

#swmmr:::OpenSwmmOutFile(out_file)
#swmmr:::CloseSwmmOutFile()

#swmmr::read_out(out_file, iType = 2, object_name = "1", vIndex = 0)
args <- list(file = out_file, iType = 0, object_name = c("1", "2"), vIndex = 0)

do.call(swmmr::read_out, args)
do.call(swmmr::read_out, c(args, firstPeriod = 200))
do.call(swmmr::read_out, c(args, firstPeriod = -1))
do.call(swmmr::read_out, c(args, firstPeriod = 35, lastPeriod = 33))
do.call(swmmr::read_out, c(args, firstPeriod = 2, lastPeriod = 12))

swmmr:::GetSwmmResultPart(iType = 1, iIndex = 1, vIndex = 1, firstPeriod = 0, lastPeriod = 0)

#swmmr:::GetSwmmResultPart(iType = 2, iIndex = 1, vIndex = 0, firstPeriod = 1, lastPeriod = 1)