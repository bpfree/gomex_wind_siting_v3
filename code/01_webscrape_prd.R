######################################################
### 01. webscrape data -- protected resources data ###
######################################################

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

# navigates to protected resources data paper (paper: https://afspubs.onlinelibrary.wiley.com/doi/full/10.1002/mcf2.10246)
# and scrapes the site data for the supporting information (https://afspubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fmcf2.10246&file=mcf210246-sup-0001-Files.zip)

# base URL
base_url <- "https://afspubs.onlinelibrary.wiley.com/doi/full/10.1002/mcf2.10246"

# navigate to page
remDr$navigate(base_url)
Sys.sleep(3)

#####################################

# prepare window
remDr$maxWindowSize()
Sys.sleep(2)

#####################################

# define web elements as an iframe so other elements can get selected
## see post on that Cloudflare uses iframe: https://stackoverflow.com/questions/76575298/how-to-click-on-verify-you-are-human-checkbox-challenge-by-cloudflare-using-se
webElem <- remDr$findElements("css", "iframe")
## make the iframe the location to find the other elements
remDr$switchToFrame(webElem[[1]])

# click "Human" toggle to get to next page (and verify that user is human)
human_button <- remDr$findElement(using = "xpath",
                                  value = "/html/body/div/div/div[1]/div/label/input")
human_button$clickElement()
Sys.sleep(3)

#####################################

# supporting information accordion control
supporting_information_dropdown <-remDr$findElement(using = "css selector",
                                                    value = "#support-information-section")
supporting_information_dropdown$clickElement()
Sys.sleep(3)

#####################################

# download zip file with supporting materials
download_toggle <-remDr$findElement(using = "css selector",
                                    value = "#mcf210246-supitem-0001 > td:nth-child(1) > a:nth-child(1)")
download_toggle$clickElement()
Sys.sleep(10)

#####################################

# close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# grab file name in downloads location
file <- list.files(download_dir, pattern = ".zip")

# move to the raw data subdirectory
file.rename(from=file.path(download_dir, file),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, file))

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

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
