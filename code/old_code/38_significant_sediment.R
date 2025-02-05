#######################################################
### 38. BOEM lease blocks with significant sediment ###
#######################################################

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
data_name <- "significant_sediment"
layer_name <- "BOEM lease blocks with significant sediment"
pattern <- "GOMSigSedBlocks"

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
#### BOEM lease blocks with significant sediment sites
data_dir <- stringr::str_glue("data/a_raw_data/{pattern}_shp")

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
## ***Note: There are a few ways to obtain the data
### 1.) BOEM Marine Mineral Mapping and Data page: https://www.boem.gov/marine-minerals/marine-minerals-mapping-and-data
### Here you can download the geodatabase or shapefile for the Gulf of Mexico or the Atlantic
###   - Geodatabase download link: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_fgdb.zip
###   - Shapefile download link: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_shp.zip
###   - Metadata: https://mmis.doi.gov/boemmmis/metadata/PlanningAndAdministration/GOMSigSedBlocks.xml

### 2.) Gulf of Mexico: https://www.boem.gov/marine-minerals/managing-multiple-uses-gulf-mexico
###   - Shapefile download link: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_shp.zip

### 3.) Marine Minerals Information System (https://mmis.doi.gov/BOEMMMIS/) -- interactive data and map portal
### Metadata: https://mmis.doi.gov/BOEMMMIS/metadata/WAF/GOMSigSedBlocks.xml
###   a.) Click the "Layers" option on the top left of the page
###   b.) Expand the "Administrative & Planning"
###   c.) Mark the "Gulf of Mexico OCS Blocks with Significant Sediment Resources"
###     ***Note: If the layer name is greyed out, zoom in further on the map till the layer name becomes black.
###              Now you can mark the layer and it should appear.
###   d.) Click the "Downloads" option on the top left of the page
###     ***Note: If too far zoomed out, page will notify you to zoom in further before being able to draw a boundary box
###   e.) Click "Select Area to Download"
###   f.) Mark boundary on the map for download area
###     ***Note: Click points on map and closing by clicking on first point.
###   g.) Once area has been chosen, choose file format (e.g., shapefile, geodatabase)
###   h.) Select "Download"
### ***NOTE: If entire dataset is desired, on layers page click the "Download Layer GDB" icon next to layer name

data <- sf::st_read(dsn = data_dir) %>%
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
  # obtain only BOEM lease blocks with significant sediment in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "BOEM lease blocks with significant sediment" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # group by ID values to flatten data
  dplyr::group_by(layer) %>%
  # summarise the grid values
  dplyr::summarise()

#####################################
#####################################

# BOEM lease blocks with significant sediment hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join BOEM lease blocks with significant sediment values to Gulf of Mexico hex cells
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

## BOEM lease blocks with significant sediment geopackage
sf::st_write(obj = data, dsn = output_gpkg, layer = stringr::str_glue("{data_name}"), apend = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), apend = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
