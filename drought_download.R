#### U.S. Drought Monitor data pull ####
# Author: Charles Scarborough
library(readr)
library(httr)
library(cli)
library(sf)
closest_date <- function(date) {
  if (is.na(try(as.Date(date, format = "%Y-%m-%d")))) {
    cli::cli_abort("{.arg date} must be in `%Y-%m-%d` format as a character string (example: 2023-01-01).")
  }
  return(as.character(seq.Date(from = as.Date("2000-01-04"), to = Sys.Date(), by = "7 days")[which.min(abs(as.numeric(difftime(time1 = as.Date(date), time2 = seq.Date(from = as.Date("2000-01-04"), to = Sys.Date()-7, by = "7 days")))))]))
}
drought_shapefile_download <- function(date) {
  if (httr::HEAD(paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_", gsub("-", "", closest_date(date = date)), "_M.zip"))$status_code == 200) {
    message(paste0("Downloading drought contours for ", closest_date(date = date)))
    temp_shp_file <- tempfile(fileext = ".zip")
    temp_shp_dir <- tempdir()
    download.file(paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_", gsub("-", "", closest_date(date = date)), "_M.zip"), destfile = temp_shp_file)
    unzip(temp_shp_file, overwrite = T, exdir = temp_shp_dir)
    drought_sf <- sf::read_sf(file.path(temp_shp_dir, paste0("USDM_", gsub("-", "", closest_date(date = date)), ".shp")))
  } else {message("Shapefile does not exist.")}
  return(drought_sf)
}
drought_statistics <- function(date, area = c("state", "county")) {
  if (area == "state") {
    if (httr::HEAD(paste0("https://droughtmonitor.unl.edu/DmData/GISData.aspx?mode=table&aoi=state&date=", gsub("-", "", closest_date(date = date))))$status_code == 200) {
      message(paste0("Downloading drought statistics for ", closest_date(date = date)))
      data <- readr::read_csv(paste0("https://droughtmonitor.unl.edu/DmData/GISData.aspx?mode=table&aoi=state&date=", closest_date(date = date)), col_types = "ccnnnnnnDD")
    } else {message("Statistics file for state does not exist.")}
  } else if (area == "county") {
    if (httr::HEAD(paste0("https://droughtmonitor.unl.edu/DmData/GISData.aspx?mode=table&aoi=county&date=", gsub("-", "", closest_date(date = date))))$status_code == 200) {
      message(paste0("Downloading drought statistics for ", closest_date(date = date)))
      data <- readr::read_csv(paste0("https://droughtmonitor.unl.edu/DmData/GISData.aspx?mode=table&aoi=county&date=", closest_date(date = date)), col_types = "ccccnnnnnnDD")
    } else {message("Statistics file for county does not exist.")}
  }
  return(data)
}