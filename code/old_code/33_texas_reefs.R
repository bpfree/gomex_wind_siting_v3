############################################
### 33. Texas permitted artificial reefs ###
############################################

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
data_name <- "texas_reefs"
layer_name <- "Texas reefs"

## setback distance (in meters)
setback <- 304.8 # equivalent to 1000 feet

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
#### Texas permitted reefs sites
data_dir <- "data/a_raw_data/tpwd-artificial-reef-data"

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
# read artificial reefs data (source: https://tpwd.texas.gov/gis/resources/tpwd-artificial-reef-data.zip)
## data are from Texas Parks and Wildlife
data <- read.csv(file.path(data_dir, "TPWD_ArtReefSites_Jan21.csv")) %>%
  # remove any observations that has NA values (only 1 occurrence)
  na.omit() %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Longitude.WGS84", "Latitude.WGS84"),
               # set the coordinate reference system to WGS84
               # ***Note: Read Me for the data states data are in decimal degrees and Web Mercator (https://epsg.io/3857)
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject the coordinate reference system
  sf::st_transform(crs) %>% # EPSG 5070 (https://epsg.io/5070)
  # create setback (buffer) of 304.8 meters (1000 feet)
  sf::st_buffer(x = .,
                dist = setback)

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
  # obtain only Texas permitted reefs in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "Texas reefs" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # group by ID values to flatten data
  dplyr::group_by(layer) %>%
  # summarise the grid values
  dplyr::summarise()

#####################################
#####################################

# Texas permitted reefs hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join Texas permitted reefs values to Gulf of Mexico hex cells
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

## Texas permitted reefs geopackage
sf::st_write(obj = data, dsn = output_gpkg, layer = stringr::str_glue("{data_name}"), apend = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), apend = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
