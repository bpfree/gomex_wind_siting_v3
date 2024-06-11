########################
### 13. NEXRAD Sites ###
########################

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
submodel <- "industry_operations"

## layer names
data_name <- "nexrad_sites"
layer_name <- "nexrad sites"
pattern <- "nexrad"

## setback distance (in meters)
setback1 <- 35000
setback2 <- 70000

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
data_dir <- "data/a_raw_data/nexrad_sites.gpkg"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_study_area.gpkg")

### output directories
#### submodel geopackage
submodel_gpkg <- stringr::str_glue("data/c_submodel_data/{submodel}.gpkg")

#### intermediate directories
output_gpkg <- stringr::str_glue("data/b_intermediate_data/{region}_{data_name}.gpkg")

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = data_dir,
              do_count = T)

sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# load data
# NEXRAD data are maintained on this site: https://www.roc.noaa.gov/WSR88D/
# To obtain the data, do the following:
## 1.) On left panel, navigate to Site ID / Maps
## 2.) Within that option, select Site ID Database
## 3.) On new page, click "Advanced Search" (page: https://www.roc.noaa.gov/WSR88D/Program/SiteID.aspx)
## 4.) Select states of interest (e.g., Louisiana and Texas) from State/Country box
##    ***Note: Can keep option as "Like"
##    ***Note: To select multiple states, click on states of interest while pressing "Control" (or equivalent of control)
## 5.) Click "Search"
## 6.) Select "Longitude" and "Latitude" as column fields
## 7.) Select "Save To Excel"

## ***Note: Longitude and latitude data are as degree-minutes-seconds, so need to convert to decimal degrees

data <- sf::st_read(dsn = data_dir,
                    # NEXRAD sites
                    layer = sf::st_layers(data_dir)[[1]][grep(pattern = stringr::str_glue("{pattern}"),
                                                                              sf::st_layers(dsn = data_dir,
                                                                                            do_count = T)[[1]])]) %>%
  # set the coordinate reference system to WGS84 [EPSG 4326 (https://epsg.io/4326)]
  sf::st_set_crs(st_crs("EPSG:4326")) %>%
  # change to correct coordinate reference system (EPSG:5070 -- NAD83 / CONUS Albers)
  sf::st_transform(x = .,
                   crs = crs)

## inspect CRS values for the data
cat(crs(data))

## Check units for determining cellsize of grid
sf::st_crs(data, parameters = TRUE)$units_gdal

#####################################

# NEXRAD sites with 35 kilometer buffer
nexrad35km <- data %>%
  #  add a setback (buffer) distance of 35km (35,000 meters)
  sf::st_buffer(dist = 35000)

# NEXRAD sites with 35 - 70 kilometer bufer
nexrad35_70km <- data %>%
  #  add a setback (buffer) distance of 70km (70,000 meters)
  sf::st_buffer(dist = 70000) %>%
  # remove the 35km buffer
  rmapshaper::ms_erase(nexrad35km)

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

# limit NEXRAD (35 - 70km) to study region
region_nexrad35_70km <- nexrad35_70km %>%
  # obtain only military training routes in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "nexrad sites" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # dissolve data by "layer" field to get as single feature
  rmapshaper::ms_dissolve(input = .,
                          field = "layer")

#####################################

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = region_nexrad35_70km, color = "blue", fill= "NA") +
  ggplot2::geom_sf(data = nexrad35_70km, color = "black", linetype = "dashed", fill = "NA")
g

g2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = study_region, color = "red", fill = NA) +
  ggplot2::geom_sf(data = nexrad35km, color = "darkblue", fill = NA) +
  ggplot2::geom_sf(data = region_nexrad35_70km, color = "blue", fill= NA) +
  ggplot2::geom_sf(data = nexrad35_70km, color = "black", linetype = "dashed", fill = NA)
g2

#####################################
#####################################

# nexrad sites hex grids
region_data_hex <- region_hex[region_nexrad35_70km, ] %>%
  # spatially join nexrad sites values to Gulf of Mexico hex cells
  sf::st_join(x = .,
              y = region_nexrad35_70km,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(GRID_ID, layer)

#####################################

# export data
## industry and operations geopackage
sf::st_write(obj = region_data_hex, dsn = submodel_gpkg, layer = stringr::str_glue("{region}_hex_{data_name}"), append = F)

## nexrad sites geopackage
sf::st_write(obj = nexrad35km, dsn = output_gpkg, layer = stringr::str_glue("{data_name}_{setback1}"), append = F)
sf::st_write(obj = nexrad35_70km, dsn = output_gpkg, layer = stringr::str_glue("{data_name}_{setback1}_{setback2}"), append = F)
sf::st_write(obj = region_nexrad35_70km, dsn = output_gpkg, layer = stringr::str_glue("{region}_{data_name}_{setback1}_{setback2}"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
