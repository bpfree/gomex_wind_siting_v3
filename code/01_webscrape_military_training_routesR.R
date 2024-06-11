######################################################
### 0. webscrape Data -- military training routes  ###
######################################################

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
zip_file <- "MTR_Segment.zip"

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

# Set directories
## download directory
download_dir <- "~/downloads"

## output directory
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Webscrape set-up
## Process uses RSelenium package (learn more about basics here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
### Another helpful tutorial: https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
### Firefox profile (based on this link: https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/)
fprof <- RSelenium::makeFirefoxProfile(list(
  # detail level for download (0 = Desktop, 1 = systems default downloads location, 2 = custom folder.)
  browser.download.folderList = 2,
  # location for data download
  browser.download.dir = data_dir,
  # stores a comma-separated list of MIME types to save to disk without asking what to use to open the file
  browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  # disable PDF javascript so that PDFs are not displayed
  pdfjs.disabled = TRUE,
  # turn off scan and loading of any additionally added plug-ins
  plugin.scan.plid.all = FALSE,
  # high number defined for version of Adobe Acrobat
  plugin.scan.Acrobat = "99.0"))

#####################################

# Launch RSelenium server and driver
rD <- RSelenium::rsDriver(browser="firefox",
                          # set which version of browser
                          version = "latest",
                          # Chrome version (turn off as Firefox will be used)
                          chromever = NULL,
                          # set which version of Gecko to use
                          geckover = "latest",
                          # status messages
                          verbose = TRUE,
                          # populate with the Firefox profile
                          extraCapabilities = fprof)

## Remote driver
remDr <- rD[["client"]]
remDr$open(silent = TRUE)

#####################################
#####################################

# Navigates to FAA military training routes (source: https://ais-faa.opendata.arcgis.com/datasets/mtr-segment-1)
# and scrapes the site data for the training routes of interest
## MarineCadastre: https://ais-faa.opendata.arcgis.com/datasets/faa::mtr-segment-1/about
## metadata: https://www.arcgis.com/sharing/rest/content/items/0c6899de28af447c801231ed7ba7baa6/info/metadata/metadata.xml?format=default&output=html

# Base URL
base_url <- "https://ais-faa.opendata.arcgis.com/datasets/mtr-segment-1"

# Navigate to page
remDr$navigate(base_url)
Sys.sleep(3)

#####################################

# Prepare window
remDr$maxWindowSize()
Sys.sleep(2)

#####################################

# Click "Download" toggle on side panel
download_toggle <-remDr$findElement(using = "css selector",
                                    value = ".hub-toolbar-inner > button:nth-child(3)")
download_toggle$clickElement()
Sys.sleep(3)

#####################################

# Click "Download" for the shapefile to get those data
## data are in a shadow form, so need to use shadowr package
shadow_rd <- shadowr::shadow(remDr)
Sys.sleep(3)

## find the calcite (shadow) buttons
shapefile_panel_button <- shadowr::find_elements(shadow_rd, 'calcite-button')

### Shapefile are the third dataset (after CSV, KML -- 4th is geojson)
shapefile_panel_button[[4]]$clickElement()

#####################################
#####################################

# Close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# Move data to correct directory
file.rename(from=file.path(download_dir, zip_file),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, zip_file))

#####################################

# Unzip the data
## grab text before ".zip" and keep only text before that
new_dir_name <- sub(".zip", "", zip_file)

## create new directory for data
new_dir <- file.path(data_dir, new_dir_name)

## unzip the file
unzip(zipfile = file.path(data_dir, zip_file),
      # export file to the new data directory
      exdir = new_dir)

## remove original zipped file
file.remove(file.path(data_dir, zip_file))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate