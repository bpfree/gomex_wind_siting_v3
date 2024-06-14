#########################
### 01. download data ###
#########################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

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
               sf,
               sp,
               stringr,
               targets,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## another common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means "use the filter function from the dplyr package"
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# Create function that will pull data from publicly available websites
## This allows for the analyis to have the most current data; for some
## of the datasets are updated with periodical frequency (e.g., every 
## month) or when needed. Additionally, this keeps consistency with
## naming of files and datasets.
### The function downloads the desired data from the URL provided and
### then unzips the data for use

data_download_function <- function(download_list, data_dir){
  
  # designate the URL that the data are hosted on
  url <- download_list
  
  # file will become last part of the URL, so will be the data for download
  file <- basename(url)
  
  # Download the data
  if (!file.exists(file)) {
    options(timeout=100000)
    # download the file from the URL
    download.file(url = url,
                  # place the downloaded file in the data directory
                  destfile = file.path(data_dir, file),
                  mode="wb")
    
    # change .kmz files to .kml files
    if (grepl(".kmz", file)){
      
      ## clam grounds
      file.rename(from=file.path(data_dir, file),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")))
      
      unzip(zipfile = file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")),
            # export file to the new data directory
            exdir = data_dir)
      
      file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, paste0(sub(".kmz", "", file), ".kml")))
      
      ## remove original zipped file
      file.remove(file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")))
    }
  }
  
  # Unzip the file if the data are compressed as .zip
  ## Examine if the filename contains the pattern ".zip"
  ### grepl returns a logic statement when pattern ".zip" is met in the file
  if (grepl(".zip", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- sub(".zip", "", file)
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Download list
download_list <- c(
  # national security submodel
  
  ## military operating areas (source: https://marinecadastre.gov/downloads/data/mc/MilitaryCollection.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::military-operating-areas/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/55364
  
  ## military training routes (source: https://marinecadastre.gov/downloads/data/mc/MilitaryCollection.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::military-training-routes/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/71812
  
  ## military special use airspace (source: https://marinecadastre.gov/downloads/data/mc/MilitaryCollection.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::military-special-use-airspace/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/48898
  
  "https://marinecadastre.gov/downloads/data/mc/MilitaryCollection.zip",
  
  #####################################
  
  # natural and cultural resources submodel
  
  ## existing coral HAPC (with and without regulations) (source: http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip)
  ### habitat areas of particular concern (HAPC) are a subset of essential fish habitat (EFH)
  ### older areas can have regulations or no regulations; newer ones under Amendment 9 might have proposed regulations or none proposed
  
  ## coral 9 HAPC (with and without regulations)(source: http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip)
  ### Amendment 9 went into effect on November 16, 2020 (read more about amendment here: https://www.govinfo.gov/content/pkg/FR-2020-10-16/pdf/2020-21298.pdf)
  
  "http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip",
  
  #####################################
  
  # industry and operations submodel
  ## automatic identification system (AIS)
  ### 2018
  #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/maps/183a8b153dec4a21ac03c6b4878b3a17/about
  #### metadata: https://www.fisheries.noaa.gov/inport/item/60419
  
  "https://marinecadastre.gov/downloads/data/ais/ais2018/AISVesselTransitCounts2018.zip",
  
  ### 2019
  #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/maps/589ba395a503447ab305e808b43074d3/about
  #### metadata: https://www.fisheries.noaa.gov/inport/item/61037
  
  "https://marinecadastre.gov/downloads/data/ais/ais2019/AISVesselTransitCounts2019.zip",
  
  ### 2020
  #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/maps/ee904bf9c45549c09c6b87a75e5a4d2c/about
  #### metadata: https://www.fisheries.noaa.gov/inport/item/64828
  
  "https://marinecadastre.gov/downloads/data/ais/ais2020/AISVesselTransitCounts2020.zip",
  
  ### 2021
  #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/maps/9bd23a442f8d4d1d9c32be993b7ca21c/about
  #### metadata: https://www.fisheries.noaa.gov/inport/item/67463
  "https://marinecadastre.gov/downloads/data/ais/ais2021/AISVesselTransitCounts2021.zip",
  
  ### 2022
  #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/maps/cc38ab7ec63549d4b01c2d65c058bcea/about
  #### metadata: https://www.fisheries.noaa.gov/inport/item/69555
  "https://marinecadastre.gov/downloads/data/ais/ais2022/AISVesselTransitCounts2022.zip",
  
  #####################################
  
  # fisheries submodel
  
  #####################################

  # logistics submodel
  
  ## principal ports (source: https://marinecadastre.gov/downloads/data/mc/PrincipalPort.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::principal-ports-2/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/56124
  
  "https://marinecadastre.gov/downloads/data/mc/PrincipalPort.zip",
  
  #####################################
  
  # wind submodel
  
  ## bathymetry
  ### central Gulf of Mexico (source: https://www.ngdc.noaa.gov/thredds/catalog/crm/cudem/catalog.html?dataset=crmDatasetScan/cudem/crm_vol4_2023.nc)
  ### western Gulf of Mexico (source: https://www.ngdc.noaa.gov/thredds/catalog/crm/cudem/catalog.html?dataset=crmDatasetScan/cudem/crm_vol5_2023.nc)
  #### NCEI: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem%3A999901/html
  #### metadata: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:999901/xml
  #### For more United States coverage and spatial resolution information, visit: https://www.ncei.noaa.gov/products/coastal-relief-model
  
  "https://www.ngdc.noaa.gov/thredds/catalog/crm/cudem/catalog.html?dataset=crmDatasetScan/cudem/crm_vol4_2023.nc",
  "https://www.ngdc.noaa.gov/thredds/catalog/crm/cudem/catalog.html?dataset=crmDatasetScan/cudem/crm_vol5_2023.nc",
  
  #####################################
  
  # economics submodel
  
  #####################################
  
  # constraints
  
  ## Rice's whale core distribution (source: https://www.fisheries.noaa.gov/s3/2021-09/shapefile_Rices_whale_core_distribution_area_Jun19_SERO.zip)
  ### NMFS: https://www.fisheries.noaa.gov/resource/map/rices-whale-core-distribution-area-map-gis-data
  ### profile: https://www.fisheries.noaa.gov/species/rices-whale
  
  "https://www.fisheries.noaa.gov/s3/2021-09/shapefile_Rices_whale_core_distribution_area_Jun19_SERO.zip",
  
  ## Louisiana permitted artificial reefs (source: https://www.wlf.louisiana.gov/page/artificial-reefs)
  ### nearshore: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Nearshore_Coordinates_Oct23.pdf
  
  "https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaNearshoreKML.kmz",
  
  ### inshore: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Inshore_Coordinates_Oct23.pdf
  
  "https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaInshoreKML.kmz",
  
  ### offshore: https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LA_Offshore_Coordinates_Oct23.pdf
  
  "https://www.wlf.louisiana.gov/assets/Fishing/Enhancing_Fish_Populations_Habitat/Files/LaOffshoreKML.kmz",
  
  ## Texas permitted artificial reefs
  
  "https://tpwd.texas.gov/gis/resources/tpwd-artificial-reef-data.zip",
  
  ## shipping fairways (source: http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip)
  ### Gulf of Mexico specific: https://www.data.boem.gov/Mapping/Files/Fairways.gdb.zip
  ### metadata: https://www.data.boem.gov/Mapping/Files/fairways_meta.html
  
  "http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip",
  
  ## unexploded ordnance (source: https://marinecadastre.gov/downloads/data/mc/MunitionsExplosivesConcern.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::munitions-and-explosives-of-concern/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/69013
  
  "https://marinecadastre.gov/downloads/data/mc/MunitionsExplosivesConcern.zip",
  
  ## offshore oil and gas active leases (source: https://marinecadastre.gov/downloads/data/mc/OffshoreOilGasActiveLease.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::offshore-oil-and-gas-active-leases/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/66158
  
  "https://marinecadastre.gov/downloads/data/mc/OffshoreOilGasActiveLease.zip",
  
  ## Gulf of Mexico OCS significant sediment blocks (source: https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_fgdb.zip)
  ### BOEM: https://www.boem.gov/marine-minerals/marine-minerals-mapping-and-data
  ### metadata: https://mmis.doi.gov/boemmmis/metadata/PlanningAndAdministration/GOMSigSedBlocks.xml
  ### map: https://www.boem.gov/sites/default/files/documents/marine-minerals/Significant%20Sediment%20Resources%20in%20the%20Gulf%20of%20Mexico.pdf
  
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
  
  "https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_fgdb.zip",
  "https://mmis.doi.gov/boemmmis/downloads/layers/GOMSigSedBlocks_shp.zip",
  
  
  ## oil and gas pipelines (source: https://www.data.boem.gov/Mapping/Files/Pipelines.gdb.zip)
  ### metadata: https://www.data.boem.gov/Mapping/Files/ppl_arcs_meta.html
  
  "https://www.data.boem.gov/Mapping/Files/Pipelines.gdb.zip",
  
  ## lightering zones (source: https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::lightering-zones/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/66149
  
  "https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip",
  
  ## anchorage areas (source: https://marinecadastre.gov/downloads/data/mc/Anchorage.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::anchorages/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/48849
  
  "https://marinecadastre.gov/downloads/data/mc/Anchorage.zip",
  
  ## aids to navigation (source: https://marinecadastre.gov/downloads/data/mc/AtoN.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::aids-to-navigation-2/about
  ### metadata: https://www.fisheries.noaa.gov/inport/item/56120
  
  "https://marinecadastre.gov/downloads/data/mc/AtoN.zip",
  
  ## BOEM drilling platform data (source: https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip)
  ### Metadata: https://www.data.boem.gov/Mapping/Files/platform_meta.html
  #### Note: These data came from the mapping page: https://www.data.boem.gov/Main/Mapping.aspx#ascii
  #### Note: These data are different from the platform query page that BOEM has: https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx
  #### That query page seems to mirror the data that BSEE also has these data
  
  "https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip",
  
  ## BOEM wind call areas data
  ### BOEM source (geodatabase): https://www.boem.gov/renewable-energy/boem-renewable-energy-geodatabase
  ### An online download link: https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data
  ### Metadata: https://www.arcgis.com/sharing/rest/content/items/709831444a234968966667d84bcc0357/info/metadata/metadata.xml?format=default&output=html
  #### ***Note: Data are also accessible for download on MarineCadastre (under "Active Renewable Energy Leases")
  #### This provides a usable URL for R: https://www.boem.gov/BOEM-Renewable-Energy-Geodatabase.zip
  
  "https://www.boem.gov/BOEM-Renewable-Energy-Geodatabase.zip"
)

#####################################
#####################################

parallel::detectCores()

cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = download_list, fun = data_download_function, data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
