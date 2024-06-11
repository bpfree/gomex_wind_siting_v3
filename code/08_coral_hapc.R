###############################
### 08. existing coral HAPC ###
###############################

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
data_name <- "coral_hapc"

data_hapc_regs <- "coral_hapc_regs"
data_hapc_noregs <- "coral_hapc_noregs"

layer_hapc_regs <- "HAPC with regulations"
layer_hapc_noregs <- "HAPC without regulations"

## geopackage name
gpkg_name <- "coral_hapc"

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
data_dir <- "data/a_raw_data/HAPCshapefiles/shpFinal2"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

### output directories
#### submodel geopackage
submodel_gpkg <- stringr::str_glue("data/c_submodel_data/{submodel}.gpkg")

#### intermediate directories
output_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_{gpkg_name}.gpkg")

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

clean_coral_function <- function(coral_data, study_region, layer_name){
  coral_layer <- coral_data %>%
    # reproject the coordinate reference system
    sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
    # obtain coral data within study region
    sf::st_intersection(study_region) %>%
    # create field called "layer" and fill with correct descriptor for summary
    dplyr::mutate(layer = layer_name) %>%
    # select key fields
    dplyr::select(layer)
  return(coral_layer)
}

#####################################
#####################################

# load data
## Load coral habitat area of particular concern (source: http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip)
## Habitat Areas of Particular Concern are a subset of Essential Fish Habitat
## Older areas can have regulations or no regulations; newer ones under Amendment 9 might have proposed regulations or none proposed
## Amendment 9 went into effect on November 16, 2020 (read more about amendment here: https://www.govinfo.gov/content/pkg/FR-2020-10-16/pdf/2020-21298.pdf)

### Coral HAPC with regulations
hapc_regs <- sf::st_read(dsn = data_dir, layer = "ExistingWithRegs") %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs)

### Coral HAPC without regulations
#### ***Note: No areas fall within study area
hapc_noregs <- sf::st_read(dsn = data_dir, layer = "ExistingWithOutRegs") %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs)

## inspect CRS values for the data
cat(crs(hapc_regs))
cat(crs(hapc_noregs))

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
## HAPC with regulations
region_hapc_regs <- clean_coral_function(coral_data = hapc_regs,
                                         study_region = study_region,
                                         layer_name = layer_hapc_regs)

## HAPC with no regulations
region_hapc_noregs <- clean_coral_function(coral_data = hapc_noregs,
                                           study_region = study_region,
                                           layer_name = layer_hapc_noregs)

## combined HAPC
region_data <- rbind(region_hapc_regs,
                     region_hapc_noregs)

#####################################
#####################################

# coral HAPC hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join coral HAPC to Gulf of Mexico hex cells
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

## coral HAPC geopackage
sf::st_write(obj = hapc_regs, dsn = output_gpkg, layer = stringr::str_glue("{data_hapc_regs}"), append = F)
sf::st_write(obj = hapc_noregs, dsn = output_gpkg, layer = stringr::str_glue("{data_hapc_noregs}"), append = F)

sf::st_write(obj = region_hapc_regs, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_hapc_regs}"), append = F)
sf::st_write(obj = region_hapc_noregs, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_hapc_noregs}"), append = F)
sf::st_write(obj = region_data, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
