################################################
### 32. Louisiana permitted artificial reefs ###
################################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region <- "gomex"

## designate submodel
submodel <- "constraints"

## layer names
data_name <- "lousiana_reefs"
layer_name <- "Lousiana reefs"

## setback distance (in meters)
setback <- 152.4 # equivalent to 500 feet

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 / Conus Albers: https://epsg.io/5070)
crs <- "EPSG:5070"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

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
## define data directory (as this is an R Project, pathnames are simplified)
### input directories
#### aids to navigation sites
data_dir <- "data/a_raw_data"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

### output directories
#### submodel geopackage
submodel_gpkg <- stringr::str_glue("data/c_submodel_data/{submodel}.gpkg")

#### intermediate directories
output_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_{data_name}.gpkg")

#####################################

# inspect layers within geopackages
sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# load data
## Lousiana inshore data

## inspect CRS values for the data
cat(crs(data))

#####################################

## study region
study_region <- sf::st_read(dsn = study_region_gpkg, layer = stringr::str_glue("{region}_study_region")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(crs = crs)

## hex grid
region_hex <- sf::st_read(dsn = study_region_gpkg, layer = stringr::str_glue("{region}_hex_grid")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(crs = crs)

#####################################
#####################################

# limit data to study region
region_data <- data %>%
  # obtain only aids to navigation in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "aids to navigation" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}"))

#####################################
#####################################

# aids to navigation hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join aids to navigation values to Westport hex cells
  sf::st_join(x = .,
              y = region_data,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(GRID_ID, layer)

#####################################
#####################################

# export data
## constraints geopackage


## aids to navigation geopackage


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate