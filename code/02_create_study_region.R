#################################
### 02. study region creation ###
#################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# designate region name
region <- "gomex"

#####################################
#####################################

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(renv,
               dplyr,
               ggplot2,
               janitor,
               plyr,
               purrr,
               rmapshaper,
               RSelenium,
               sf,
               sp,
               stringr,
               targets,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# set directories
## input geodatabase
data_dir <- "data/a_raw_data/GoMex 2024.gdb"

## output geopackage
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

#####################################

sf::st_layers(dsn = data_dir,
              do_count = T)

#####################################
#####################################

# load data
## submodel data (has hexes)
hex_grid <- sf::st_read(dsn = data_dir,
                        layer = sf::st_layers(data_dir)[[1]][grep(pattern = "Constraints",
                                                                  sf::st_layers(dsn = data_dir, do_count = T)[[1]])]) %>%
  dplyr::select(GRID_ID)

#####################################

# Gulf of Mexico version 3 call area hexes as single feature
## ***Note: This dataset will be used to extract any data from datasets
##          within the model that will impact wind siting suitability
study_region <- hex_grid %>%
  # create field called "call area"
  dplyr::mutate(call_area = "call_area") %>%
  # group all rows by the different elements with "call area" field -- this will create a row for the grouped data
  dplyr::group_by(call_area) %>%
  # summarise all those grouped elements together -- in effect this will create a single feature
  dplyr::summarise()

sf::st_crs(study_region, parameters = TRUE)
cat(crs(study_region))

#####################################
#####################################

# export study region
sf::st_write(obj = hex_grid, dsn = study_region_gpkg, layer = stringr::str_glue("{region}_hex_grid"), append = F)
sf::st_write(obj = study_region, dsn = study_region_gpkg, layer = stringr::str_glue("{region}_study_region"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
