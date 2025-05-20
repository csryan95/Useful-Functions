#### AirNow data pull ####
# Author: Charles Scarborough
library(cli)
library(tidyverse)
library(httr)
library(readr)
read_airnow_data <- function(start_date, end_date, aqs_sites) {
  if (is.na(try(as.Date(start_date, format = "%Y-%m-%d")))) {
    cli::cli_abort("{.arg start_date} must be in `%Y-%m-%d` format as a character string (example: 2023-01-01).")
  }
  if (is.na(try(as.Date(end_date, format = "%Y-%m-%d")))) {
    cli::cli_abort("{.arg end_date} must be in `%Y-%m-%d` format as a character string (example: 2023-01-01).")
  }
  if (as.Date(start_date) > as.Date(end_date)) {
    cli::cli_abort("{.arg start_date} cannot later than {.arg end_date}.")
  }
  if (length(unique(nchar(aqs_sites))) != 1 || unique(nchar(aqs_sites) != 9)) {
    cli::cli_abort("{.arg aqs_sites} may be formatted incorrectly. Full AQS site codes must be in [state code, county code, site ID] format (example: 320030073; state code = 32, county code = 003, site ID = 0073).")
  }
  if (start_date == end_date) {
    date_range <- c(as.character(as.Date(start_date) - 1), start_date, as.character(as.Date(start_date) + 1))
  } else if (start_date != end_date) {
    date_range <- as.character(seq.Date(from = as.Date(start_date) - 1, to = as.Date(end_date) + 1, by = "day"))
  }
  if (length(date_range) > 5) {
    message("Pulling a lot of data. Please be patient.")
  }
  f <- function(df, pos) { 
    df %>% dplyr::filter(aqs_sitecode %in% aqs_sites)
  }
  data <- data.frame()
  i <- 0
  message("Beginning data pull")
  for (d in date_range) {
    for (h in 0:23) {
      if (httr::HEAD(paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/", format(as.Date(d), "%Y"), "/", gsub(pattern = "-", "", d), "/", "HourlyData_", gsub(pattern = "-", "", d), stringr::str_pad(string = as.character(h), width = 2, side = "left", pad = "0"), ".dat"))$status_code == 200) {
        i <- i + 1
        df <- readr::read_delim_chunked(file = paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/", format(as.Date(d), "%Y"), "/", gsub(pattern = "-", "", d), "/", "HourlyData_", gsub(pattern = "-", "", d), stringr::str_pad(string = as.character(h), width = 2, side = "left", pad = "0"), ".dat"),
                                        callback = DataFrameCallback$new(f), 
                                        chunk_size = 10000,  
                                        delim = "|", 
                                        progress = F,
                                        col_names = c("date", "time", "aqs_sitecode", "sitename", "gmt_offset", "parameter", "units", "value", "data_source"), 
                                        col_types = "ccccnccnc")
        data <- dplyr::bind_rows(data, df)
        rm(df)
      } else {next}
    }
    message(paste0(round((i/(length(0:23)*length(date_range)))*100, digits = 1), "% complete"))
  }
  message(paste0("Pulled ", round(as.numeric(abs(difftime(time1 = start_date, time2 = end_date))), digits = 0), " days of data."))
  data$datetime <- as.POSIXct(paste(data$date, data$time, sep = " "), "%m/%d/%y %H:%M", tz = "UTC")
  data$datetime_lst <- data$datetime + (data$gmt_offset*60*60)
  data$hour <- as.numeric(format(data$datetime_lst, "%H"))
  data$date <- as.Date(data$datetime_lst)
  data$date_char <- as.character(data$date)
  data$parameter <- tolower(data$parameter)
  data$units <- tolower(data$units)
  data <- data %>%
    dplyr::filter(dplyr::between(date, left = as.Date(start_date), right = as.Date(end_date))) %>%
    dplyr::select(date, hour, datetime = datetime_lst, aqs_sitecode, sitename, parameter, units, value, data_source)
  return(data)
}
# test <- read_airnow_data(start_date = "2022-02-01", end_date = "2022-02-01", aqs_sites = c("320030540", "320030561", "320030298", "320030299", "320030602", "320031019", "320030024", "320030043", "320030044", "320030071", "320030073", "320030075", "320031502", "320032003"))

pivot_wind <- function(data, parameters = NULL) {
  if (!is.null(parameters) & length(lubridate::setdiff(parameters, unique(data$parameter))) > 0) {
    cli::cli_abort("{.arg parameters} must exist in {.arg data}. Try `unique(data$parameter)` to find available parameters.")
  }
  if (is.null(parameters)) {
  data <- data %>%
    tidyr::pivot_wider(names_from = parameter,
                       values_from = c(units, value)) %>%
    tidyr::fill(dplyr::all_of(grep("units", colnames(.), value = T)), .direction = "downup")
  } else {
    data <- data %>%
      dplyr::filter(parameter %in% c("rwd", "rws", "ws", "wd", parameters)) %>%
      tidyr::pivot_wider(names_from = parameter,
                         values_from = c(units, value)) %>%
      tidyr::fill(dplyr::all_of(grep("units", colnames(.), value = T)), .direction = "downup")
  }
  return(data)
}

# test_pivot <- pivot_wind(data = test, parameters = c("pm10", "pm2.5", "no2"))
