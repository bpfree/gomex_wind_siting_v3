#######################################
### 01. webscrape data -- platforms ###
#######################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# data name
data_name <- "platform"

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
## download directory
download_dir <- "~/downloads"

## output directory
data_dir <- "data/a_raw_data"

#####################################
#####################################

# webscrape set-up
## process uses RSelenium package (learn more about basics here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
### another helpful tutorial: https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
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

# launch RSelenium server and driver
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

## remote driver
remDr <- rD[["client"]]
remDr$open(silent = TRUE)

#####################################
#####################################

# navigates to BOEM platform dataset (source: https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx)
# and scrapes the site data for the states and regions of interest
## field definitions: https://www.data.bsee.gov/Platform/PlatformStructures/FieldDefinitions.aspx

# load platform data (source: https://www.data.boem.gov/Main/Platform.aspx)

# base URL
base_url <- "https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx"

# navigate to page
remDr$navigate(base_url)
Sys.sleep(5)

#####################################

# prepare window
remDr$maxWindowSize()
Sys.sleep(2)

#####################################

# click link to download mappackage
csv_toggle <-remDr$findElement(using = "css selector",
                               value = "#ASPxFormLayout2_btnCsvExport")
csv_toggle$clickElement()

# wait 5 seconds (may need to change value depending on Internet speeds)
Sys.sleep(5)

#####################################
#####################################

# close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# view files in downloads folder to make sure file is there
list.files(download_dir, pattern = ".csv")

file.rename(from=file.path(download_dir, list.files(download_dir, pattern = ".csv")),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, stringr::str_glue("{data_name}.csv")))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
