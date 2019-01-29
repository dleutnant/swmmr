package_file <- function(...) system.file(..., package = "swmmr")

inp_file <- package_file("extdata", "Example1.inp")

exec <- "/home/hauke/CProgramming/Stormwater-Management-Model/bin/run-swmm"

out_file <- file.path(tempdir(), "swmm-Example1.out")

swmmr::run_swmm(inp_file, out = out_file, exec = exec)

swmmr:::OpenSwmmOutFile(out_file)
swmmr:::CloseSwmmOutFile()

swmmr::read_out(out_file, iType = 2, object_name = "1", vIndex = 0)
swmmr:::GetSwmmResultPart(iType = 2, iIndex = 1, vIndex = 0, firstPeriod = 1, lastPeriod = 1)