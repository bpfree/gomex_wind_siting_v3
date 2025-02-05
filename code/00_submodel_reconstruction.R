######################
### 00. Clean data ###
######################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "gom"

## version
version <- "v3"

## coordinate reference system
### set the coordinate reference system that data should become (: https://epsg.io/)
crs <- "EPSG:"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

## data path
submodel_data <- "data/c_submodel_data/NCCOS_Model"

#####################################
#####################################

librarian::shelf(tidyverse,
                 sf)

#####################################
#####################################

# geodatabase pathnames

natsec_gdb <- file.path(submodel_data, "WEA_GOM3_submodels.gdb")
natcul_gdb <- file.path(submodel_data, "Natural_Resources.gdb")
idstry_gdb <- file.path(submodel_data, "Industry.gdb")
fish_gdb <- file.path(submodel_data, "Fisheries.gdb")
wind_gdb <- file.path(submodel_data, "Wind.gdb")

### output directory
output_gpkg <- stringr::str_glue("data/c_submodel_data/{region_name}_{version}_submodel_{date}.gpkg")

#####################################
#####################################

sf::st_layers(dsn = natsec_gdb,
              do_count = FALSE)

sf::st_layers(dsn = natcul_gdb,
              do_count = FALSE)

sf::st_layers(dsn = idstry_gdb,
              do_count = FALSE)

sf::st_layers(dsn = fish_gdb,
              do_count = FALSE)

sf::st_layers(dsn = wind_gdb,
              do_count = FALSE)

#####################################
#####################################

gom_hex <- sf::st_read(dsn = natsec_gdb,
                       layer = sf::st_layers(dsn = natsec_gdb)[[1]][[grep(pattern = "natsec",
                                                                          x = sf::st_layers(dsn = natsec_gdb)[[1]])]]) %>%
  dplyr::select(contains("GRID"))

natsec_df <- sf::st_read(dsn = natsec_gdb,
                         layer = sf::st_layers(dsn = natsec_gdb)[[1]][[grep(pattern = "natsec",
                                                                            x = sf::st_layers(dsn = natsec_gdb)[[1]])]]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(1:5)

natcul_df <- sf::st_read(dsn = natcul_gdb,
                         layer = sf::st_layers(dsn = natcul_gdb)[[1]][[grep(pattern = "N_5$",
                                                                            x = sf::st_layers(dsn = natcul_gdb)[[1]])]]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(!contains("Shape"))

idstry_df <- sf::st_read(dsn = idstry_gdb,
                         layer = sf::st_layers(dsn = idstry_gdb)[[1]][[grep(pattern = "3_5$",
                                                                            x = sf::st_layers(dsn = idstry_gdb)[[1]])]]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(!contains("Shape"))

fish_df <- sf::st_read(dsn = fish_gdb,
                         layer = sf::st_layers(dsn = fish_gdb)[[1]][[grep(pattern = "3_5$",
                                                                            x = sf::st_layers(dsn = fish_gdb)[[1]])]]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(!contains("Shape"))

wind_df <- sf::st_read(dsn = wind_gdb,
                       layer = sf::st_layers(dsn = wind_gdb)[[1]][[grep(pattern = "3_5$",
                                                                        x = sf::st_layers(dsn = wind_gdb)[[1]])]]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(!contains("Shape"))

#####################################
#####################################

submodel_complete <- gom_hex %>%
  dplyr::left_join(x = .,
                   y = natsec_df,
                   by = "GRID_ID") %>%
  dplyr::left_join(x = .,
                   y = natcul_df,
                   by = "GRID_ID") %>%
  dplyr::left_join(x = .,
                   y = idstry_df,
                   by = "GRID_ID") %>%
  dplyr::left_join(x = .,
                   y = fish_df,
                   by = "GRID_ID") %>%
  dplyr::left_join(x = .,
                   y = wind_df,
                   by = "GRID_ID")

#####################################
#####################################

# export data
sf::st_write(obj = submodel_complete, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{version}_submodel_{date}"), append = F)
