## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE)

## ----load_libs, message= FALSE-------------------------------------------
# First load the following packages:
library(swmmr)
library(purrr)
library(dplyr)
library(sf)

## ----model_preparation---------------------------------------------------
# in case your operating system is Windows, the examples are saved in the following directory:
# "C:/Users/.../Documents/EPA SWMM Projects/Examples/"
# please change the path to:
# "C:/Users/.../Documents/EPA_SWMM_Projects/Examples/"
# substitute '...' with your user name

## ----model_setup---------------------------------------------------------
# set path to inp
# If your operating system is Windows, the Example1.inp model is usually 
# located at "C:\Users\your user name\Documents\EPA SWMM Projects\Examples".
# For convenience the Example1.inp model is also included in the swmmr package.
# Feel free to change this to your path of choice.
inp_file <- system.file("extdata", "Example1.inp", package = "swmmr", mustWork = TRUE)

## ----out_dir-------------------------------------------------------------
# set the path to the output directory for the example files (here, we use a temp directory)
# Feel free to change this to your path of choice. 
out_dir <- tempdir()

## ----inp_to_files--------------------------------------------------------
# read the "Example1.inp" using read_inp
Example1 <- read_inp(x = inp_file)

# glance the structure of Example1
summary(Example1)

# convert .inp file into independent .shp and .txt files
inp_to_files(x = Example1, name = "Example1", path_out = out_dir)

# check folders in your output directory:
list.files(out_dir)

# check existence of shape, text and dat files:
c("shp", "txt", "dat") %>% 
  map( ~ file.path(out_dir, .)) %>% 
  map(list.files)


## ----shp_to_inp----------------------------------------------------------
# convert shp and txt files to inp object:
Example1_con <- shp_to_inp(
  path_options = file.path(out_dir,"txt/Example1_options.txt"),
  path_timeseries = file.path(out_dir,"dat/Example1_timeseries_TS1.dat"),
  path_polygon = file.path(out_dir,"shp/Example1_polygon.shp"),
  path_line = file.path(out_dir,"shp/Example1_link.shp"),
  path_point = file.path(out_dir,"shp/Example1_point.shp"),
  path_outfall = file.path(out_dir,"shp/Example1_outfall.shp")
)

# glance the structure of the converted Example1
summary(Example1_con)

# save the input file to a new folder in the output directory:
dir.create(file.path(out_dir, "inp_new"))
write_inp(Example1_con, file.path(out_dir, "inp_new", "Example1_con.inp"))

## ----objects-------------------------------------------------------------
# ... assuming infiltration parameters are not given in the .shp file, an R object (tibble or data.frame) called infiltration can be added. Additionally a column 'Soil' must be added to polygon shp file.

infiltration <- tibble(
  Soil = c("A", "B"), # or: unique(polygon$Soil)
  MaxRate = c(76.2, 127), 
  MinRate = c(3.81, 7.62),
  Decay = c(0.069, 0.069),
  DryTime = c(1,1),
  MaxInf = c(0,0)
)
  
# ... assuming not all subcatchment related parameters are given in the polygon .shp-file, an R object (tibble or data.frame) called subcatchment_typologies can be added. Additionally a column 'Type' must be added to the polygon .shp file. 

subcatchment_typologies <- tibble(
  Type = c("Street", "Park"), # or: unique(polygon$Type)
  Perc_Imperv = c(100, 10),
  Width = c(9, 30),
  Slope = c(0.57, 1),
  CurbLen = 0,
  Snowpack = ' ',
  Rain_Gage = "Test_rain",
  N_Imperv = c(0.01, 0.025),
  N_Perv = c(0.01, 0.2),
  S_Imperv = c(1.5, 0.58),
  S_Perv = c(1.5, 0.58),
  Pct_Zero = 0,
  PctRouted = 100
)

#...assuming roughness is not given in the line .shp file, an R object (tibble or data.frame) called conduit_material can be added. Additionally a column 'Material' must be added to the line .shp file
conduit_material <- tibble(
  Material = "B", # or: unique(lines$Material)
  Roughness = 0.018
)

#... assuming surcharge of junctions should be added later:
junction_parameters <- tibble(
  Y = 0,
  Ysur = 1,
  Apond = 1
)


