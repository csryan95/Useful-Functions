#### Hazard Mapping System (HMS) data pull ####
# Author: Charles Scarborough
library(readr)
library(httr)
library(cli)
library(sf)
smoke_download_function <- function(date) {
  if (is.na(try(as.Date(date, format = "%Y-%m-%d")))) {
    cli::cli_abort("{.arg date} must be in `%Y-%m-%d` format as a character string (example: 2023-01-01).")
  }
  if (as.Date(date, format = "%Y-%m-%d") >= Sys.Date()) {
    cli::cli_abort("{.arg date} must be less than the current date.")
  }
  if (httr::HEAD(paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", 
                        format(as.Date(date, format = "%Y-%m-%d"), "%Y"), "/",
                        format(as.Date(date, format = "%Y-%m-%d"), "%m"), "/",
                        "hms_smoke",
                        gsub("-", "", as.character(date)),
                        ".zip"))$status_code == 200) {
    message(paste0("Downloading HMS smoke contours for ", date))
    temp_shp_file <- tempfile(fileext = ".zip")
    temp_shp_dir <- tempdir()
    download.file(paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", 
                         format(as.Date(date, format = "%Y-%m-%d"), "%Y"), "/",
                         format(as.Date(date, format = "%Y-%m-%d"), "%m"), "/",
                         "hms_smoke",
                         gsub("-", "", as.character(date)),
                         ".zip"), destfile = temp_shp_file)
    unzip(temp_shp_file, overwrite = T, exdir = temp_shp_dir)
    smoke_sf <- sf::read_sf(file.path(temp_shp_dir, paste0("hms_smoke", gsub("-", "", as.character(date)), ".shp")))
  } else {message("Shapefile does not exist.")}
  return(smoke_sf)
}
fire_download_function <- function(date) {
  if (is.na(try(as.Date(date, format = "%Y-%m-%d")))) {
    cli::cli_abort("{.arg date} must be in `%Y-%m-%d` format as a character string (example: 2023-01-01).")
  }
  if (as.Date(date, format = "%Y-%m-%d") >= Sys.Date()) {
    cli::cli_abort("{.arg date} must be less than the current date.")
  }
  if (httr::HEAD(paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", 
                        format(as.Date(date, format = "%Y-%m-%d"), "%Y"), "/",
                        format(as.Date(date, format = "%Y-%m-%d"), "%m"), "/",
                        "hms_fire",
                        gsub("-", "", as.character(date)),
                        ".zip"))$status_code == 200) {
    message(paste0("Downloading HMS smoke contours for ", date))
    temp_shp_file <- tempfile(fileext = ".zip")
    temp_shp_dir <- tempdir()
    download.file(paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", 
                         format(as.Date(date, format = "%Y-%m-%d"), "%Y"), "/",
                         format(as.Date(date, format = "%Y-%m-%d"), "%m"), "/",
                         "hms_fire",
                         gsub("-", "", as.character(date)),
                         ".zip"), destfile = temp_shp_file)
    unzip(temp_shp_file, overwrite = T, exdir = temp_shp_dir)
    fire_sf <- sf::read_sf(file.path(temp_shp_dir, paste0("hms_fire", gsub("-", "", as.character(date)), ".shp")))
  } else {message("Shapefile does not exist.")}
  return(fire_sf)
}