####################################
### 37. oil and gas active lease ###
####################################

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
data_name <- "oil_gas_active_lease"
layer_name <- "oil and gas active lease"
pattern <- "OffshoreOilGasActiveLease"

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
#### oil and gas active lease sites
data_dir <- stringr::str_glue("data/a_raw_data/{pattern}/{pattern}.gpkg")

#### study area grid
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

### output directories
#### submodel geopackage
submodel_gpkg <- stringr::str_glue("data/c_submodel_data/{submodel}.gpkg")

#### intermediate directories
output_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_{data_name}.gpkg")

#####################################

# inspect layers within geopackages
sf::st_layers(dsn = data_dir,
              do_count = T)

sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# load data
## read offshore oil and gas active leases (source: https://marinecadastre.gov/downloads/data/mc/OffshoreOilGasActiveLease.zip)
### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::offshore-oil-and-gas-active-leases/about
### metadata: https://www.fisheries.noaa.gov/inport/item/66158
data <- sf::st_read(dsn = data_dir,
                    # oil and gas active lease
                    layer = sf::st_layers(dsn = data_dir)[[1]][grep(pattern = stringr::str_glue("{pattern}"),
                                                                    x = sf::st_layers(dsn = data_dir)[[1]])]) %>%
  # reproject the coordinate reference system
  sf::st_transform(crs) # EPSG 5070 (https://epsg.io/5070)

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
  # obtain only oil and gas active lease in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "oil and gas active lease" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # group by ID values to flatten data
  dplyr::group_by(layer) %>%
  # summarise the grid values
  dplyr::summarise()

#####################################
#####################################

# oil and gas active lease hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join oil and gas active lease values to Gulf of Mexico hex cells
  sf::st_join(x = .,
              y = region_data,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(GRID_ID, layer)

#####################################
#####################################

# export data
## constraints geopackage
sf::st_write(obj = region_data_hex, dsn = submodel_gpkg, layer = stringr::str_glue("{region}_hex_{data_name}"), append = F)

## oil and gas active lease geopackage
sf::st_write(obj = data, dsn = output_gpkg, layer = stringr::str_glue("{data_name}"), apend = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), apend = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
