#################################
### 14. oil and gas boreholes ###
#################################

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
data_name <- "boreholes"
layer_name <- "boreholes"

## setback distance (in meters)
setback <- 60.96 # equivalent to 500 feet

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
#### borehole sites
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
sf::st_layers(dsn = data_dir,
              do_count = T)

sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# load data
# Load borehole data (source: https://www.data.boem.gov/Well/Borehole/Default.aspx)
## Query Definitions: https://www.data.boem.gov/Well/Borehole/Default.aspx
## Metadata / Field Definitions: https://www.data.boem.gov/Main/HtmlPage.aspx?page=borehole
## Field Values: https://www.data.boem.gov/Main/HtmlPage.aspx?page=boreholeFields
### Borehole Status Code
####   1.) APD -- Application for permit to drill
####   2.) AST -- Approved sidetrack
####   3.) BP -- Bypass
####   4.) CNL -- Borehole is cancelled. The request to drill the well is cancelled after the APD or sundry has been approved. The status date of the borehole was cancelled.
####   5.) COM -- Borehole completed
####   6.) CT -- Core test well
####   7.) DRL -- Drilling active
####   8.) DSI -- Drilling suspended
####   9.) PA -- Permanently abandoned
####   10.) ST -- Borehole side tracked
####   11.) TA -- temporarily abandoned
####   12.) VCW -- Volume chamber well

### Type Code
####   1.) C -- Core test
####   2.) D -- Development
####   3.) E -- Exploratory
####   4.) N -- Non-operation
####   5.) O -- Other
####   6.) R -- Relief
####   7.) S -- Strat test

## Data were up-to-date as of 05 June 2024
data <- read.csv(paste(data_dir, "borehole.csv", sep = "/")) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Surface.Longitude", "Surface.Latitude"),
               # According to BOEM, coordinate data are in NAD27 (EPSG:4267)
               crs = 4267) %>% # EPSG:4267 (https://epsg.io/4267)
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # remove any boreholes that have been side tracked or permanently abandoned
  # return all not status codes CNL, PA and ST
  dplyr::filter(!Status.Code %in% c("CNL", "PA", "ST")) %>%

  # add a setback (buffer) distance of 60.96 meters (200 feet) around the boreholes
  sf::st_buffer(dist = setback)

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
  # obtain only borehole in the study area
  rmapshaper::ms_clip(target = .,
                      clip = study_region) %>%
  # create field called "layer" and fill with "borehole" for summary
  dplyr::mutate(layer = stringr::str_glue("{layer_name}")) %>%
  # group by ID values to flatten data
  dplyr::group_by(layer) %>%
  # summarise the grid values
  dplyr::summarise()

#####################################
#####################################

# borehole hex grids
region_data_hex <- region_hex[region_data, ] %>%
  # spatially join borehole values to Gulf of Mexico hex cells
  sf::st_join(x = .,
              y = region_data,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(GRID_ID, layer)

#####################################
#####################################

# export data


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
