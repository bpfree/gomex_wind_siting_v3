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
data_name <- "louisiana_reefs"
layer_name <- "Louisiana reefs"

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
#### Louisiana permitted reefs sites
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
## Louisiana inshore data (source: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaInshoreKML.kmz)
### PDF: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Inshore_Coordinates_Oct23.pdf
la_inshore_reef <- sf::st_read(dsn = file.path(data_dir, layer = "LaInshoreKML.kml")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs) %>%
  # apply 152.4-m setback
  sf::st_buffer(x = .,
                dist = setback) %>%
  # drop Z dimension
  sf::st_zm(x = .)

## Louisiana nearshore data (source: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaNearshoreKML.kmz)
### PDF: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Nearshore_Coordinates_Oct23.pdf
la_nearshore_reef <- sf::st_read(dsn = file.path(data_dir, layer = "LaNearshoreKML.kml")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs) %>%
  # apply 152.4-m setback
  sf::st_buffer(x = .,
                dist = setback) %>%
  # drop Z dimension
  sf::st_zm(x = .)

## Louisiana offshore data (source: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaOffshoreKML.kmz)
### PDF: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Offshore_Coordinates_Oct23.pdf
la_offshore_reef <- sf::st_read(dsn = file.path(data_dir, layer = "LaOffshoreKML.kml")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs) %>%
  # apply 152.4-m setback
  sf::st_buffer(x = .,
                dist = setback) %>%
  # drop Z dimension
  sf::st_zm(x = .)

## inspect CRS values for the data
cat(crs(la_inshore_reef))
cat(crs(la_nearshore_reef))
cat(crs(la_offshore_reef))

#####################################

la_reef <- rbind(la_inshore_reef,
                 la_nearshore_reef,
                 la_offshore_reef)

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
region_data <- la_reef %>%
  # obtain only Louisiana reefs in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "Louisiana reefs" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # group by ID values to flatten data
  dplyr::group_by(layer) %>%
  # summarise the grid values
  dplyr::summarise()

#####################################
#####################################

# Louisiana permitted artificial reefs hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join permitted artificial reefs values to Gulf of Mexico hex cells
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

## Louisiana reefs geopackage
sf::st_write(obj = la_inshore_reef, dsn = output_gpkg, layer = stringr::str_glue("{data_name}_inshore"), append = F)
sf::st_write(obj = la_nearshore_reef, dsn = output_gpkg, layer = stringr::str_glue("{data_name}_nearshore"), append = F)
sf::st_write(obj = la_offshore_reef, dsn = output_gpkg, layer = stringr::str_glue("{data_name}_offshore"), append = F)
sf::st_write(obj = la_reef, dsn = output_gpkg, layer = stringr::str_glue("{data_name}"), append = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
