##############################
### 11. PRD combined layer ###
##############################

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
submodel <- "natural_cultural"

## layer names
data_name <- "prd_combined"
layer_name <- "protected resources combined"

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
#### coral HAPC
data_dir <- "data/a_raw_data/mcf210246-sup-0001-files"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

### output directories
#### submodel geopackage
submodel_gpkg <- stringr::str_glue("data/c_submodel_data/{submodel}.gpkg")

#### intermediate directories
output_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_{data_name}.gpkg")

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# load data
## Load protected resources data combined layer (source: https://afspubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fmcf2.10246&file=mcf210246-sup-0001-Files.zip)
### paper: https://afspubs.onlinelibrary.wiley.com/doi/full/10.1002/mcf2.10246

data <- sf::st_read(dsn = data_dir) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs)

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
  # obtain only military training routes in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "military special use airspace" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}"))

#####################################
#####################################

# military special use airspace hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join military special use airspace values to Gulf of Mexico hex cells
  sf::st_join(x = .,
              y = region_data,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(GRID_ID, layer, PRODUCT) %>%
  # group by the ID values as there are duplicates
  dplyr::group_by(GRID_ID) %>%
  # summarise the PRD combined layer by minimum of product
  ## take the minimum value of the PRD for any that overlap
  ## ***Note: this will provide the most conservation given that low
  ##          values are less desirable
  dplyr::summarise(prd_index = min(PRODUCT))

#####################################
#####################################

# export data
## constraints geopackage
sf::st_write(obj = region_data_hex, dsn = submodel_gpkg, layer = stringr::str_glue("{region}_hex_{data_name}"), append = F)

## PRD geopackage
sf::st_write(obj = data, dsn = output_gpkg, layer = stringr::str_glue("{data_name}"), append = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
