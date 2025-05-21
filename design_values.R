# Author: Charles Scarborough
# Contact Information: cscarborough@sonomatech.com
# Created: May, 2025
# Load libraries
library(tidyverse)
library(janitor)
rm(list = ls())

#### NO2 ####

no2_1hr <- daily_data_sub %>%
  filter(parameter == "Nitrogen dioxide (NO2)", validity_indicator == "Y", pollutant_standard == "NO2 1-hour 2010") %>%
  mutate(year = as.numeric(format(date_local, "%Y"))) %>%
  filter(year %in% c(2021, 2022, 2023)) %>%
  group_by(site, poc, year, pct_complete) %>%
  reframe(pctile_98 = quantile(first_max_value, probs = 0.98)) %>%
  ungroup() %>%
  group_by(site, poc) %>%
  reframe(no2_primary_1hr_dv = mean(pctile_98),
          comp_index = sum(pct_complete >= 75)) %>%
  ungroup()

no2_annual <- sample_data_sub %>%
  filter(parameter == "Nitrogen dioxide (NO2)", !is.na(sample_measurement)) %>%
  mutate(year = as.numeric(format(date_local, "%Y"))) %>%
  filter(year %in% c(2021, 2022, 2023)) %>%
  group_by(site, poc, year, pct_complete) %>%
  reframe(mean = mean(sample_measurement)) %>%
  ungroup()

#### Ozone ####
# requirement: daily ozone data downloaded from AQS
# Annual fourth-highest daily maximum 8-hour concentration, averaged over 3 years
# level: 0.070 ppm

tidy_aqs_daily_data_ozone <- function(dailyData,
                                      standard_val = 0.070,
                                      duration = '8-HR RUN AVG BEGIN HOUR',
                                      standard = 'Ozone 8-hour 2015') {
  df <- dailyData %>%
    filter(sample_duration == duration,
           pollutant_standard == standard,
           event_type %in% c('No Events', 'Concurred Events Excluded')) %>%
    mutate(obs_pct_include = case_when(observation_percent < 75 & first_max_value >= standard_val ~ 'Y',
                                       observation_percent >= 75 ~ 'Y',
                                       TRUE ~ 'N')) %>%
    filter(obs_pct_include == 'Y') %>%
    select(aqs_sitecode, poc, parameter_code, parameter, 
           sample_duration, date_local, observation_percent, 
           first_max_value, first_max_hour, 
           local_site_name, year, event_type, observation_percent,
           validity_indicator) %>%
    mutate(first_max_value = first_max_value,
           units_of_measure = 'PPM')
  return(df)
}
data_events_excluded <- tidy_aqs_daily_data(dailyData = o3, 
                                            standard_val = 0.070,
                                            event_type_filter = c("No Events", "Concurred Events Excluded"))

calculate_design_values_o3 <- function(dailyData, 
                                       end_yr, 
                                       dates_to_remove = NA,
                                       end_yr_partial = FALSE,
                                       num_years = 3) {
  
  start_yr <- end_yr - (num_years - 1)
  
  if (!is.na(dates_to_remove)) {
    for (s in names(dates_to_remove)) {
      dailyData <- dailyData %>%
        filter(!(aqs_sitecode == s & date_local %in% dates_to_remove[[s]]))
    }
  }
  
  top10v_dat <- dailyData %>%
    filter(between(year, start_yr, end_yr)) %>%
    group_by(aqs_sitecode, year) %>%
    slice_max(order_by = first_max_value, n = 20) %>% 
    mutate(rank = dense_rank(desc(first_max_value))) %>%
    arrange(desc(first_max_value), .by_group = T) %>% 
    select(aqs_sitecode, year, first_max_value, date_local, rank,
           event_type, observation_percent, validity_indicator) %>%
    ungroup()
  
  # browser()
  
  fourmv_dat <- dailyData %>%
    filter(between(year, start_yr, end_yr)) %>%
    group_by(aqs_sitecode, year) %>%
    slice_max(order_by = first_max_value, n = 4, with_ties = F) %>%
    slice_min(order_by = first_max_value, n = 1, with_ties = F) %>%
    rename(fourth_highest = first_max_value) %>%
    select(aqs_sitecode, year, fourth_highest) %>%
    distinct() %>%
    ungroup()
  # fourmv_dat <- dailyData %>%
  #   filter(between(year, start_yr, end_yr)) %>%
  #   group_by(aqs_sitecode, year) %>%
  #   slice_max(order_by = first_max_value, n = 15) %>%
  #   mutate(rank = dense_rank(desc(first_max_value))) %>%
  #   ungroup() %>%
  #   filter(rank == 4) %>%
  #   # slice_min(order_by = first_max_value, n = 1) %>%
  #   rename(fourth_highest = first_max_value) %>%
  #   select(aqs_sitecode, year, fourth_highest) %>%
  #   distinct() # %>%
  # #ungroup()
  
  dv <- fourmv_dat %>%
    group_by(aqs_sitecode) %>%
    # summarize(dv = round(mean(fourth_highest), 3))
    summarize(dv = as.character(mean(fourth_highest)*1000)) %>% 
    mutate(dv = gsub("\\..*$", "", dv)) %>% 
    mutate(dv = as.numeric(dv)/1000)
  
  fourmv_pivot <- fourmv_dat %>% pivot_wider(id_cols = 'aqs_sitecode',
                                             names_from = 'year', 
                                             values_from = 'fourth_highest')
  
  return(list('dv' = dv, 'fourth_highest' = fourmv_pivot, 'top10values' = top10v_dat))
  
}

dvs_events_excluded <- calculate_design_values_o3(dailyData = data_events_excluded, 
                                                  end_yr = 2023, 
                                                  dates_to_remove = NA, 
                                                  end_yr_partial = F, 
                                                  num_years = 3)

#### Lead ####
# requirement: daily lead data downloaded from AQS

max_3_month_mean <- daily_lead %>%
  filter(event_type %in% c("No Events", "Concurred Events Excluded")) %>%
  select(aqs_sitecode, date_local, year, parameter, arithmetic_mean) %>%
  mutate(month = as.numeric(format(date_local, "%m"))) %>%
  group_by(aqs_sitecode, year, month, parameter) %>%
  reframe(arithmetic_mean = mean(arithmetic_mean, na.rm = T)) %>%
  ungroup() %>%
  arrange(aqs_sitecode, parameter, year, month) %>%
  group_by(aqs_sitecode, parameter) %>%
  mutate(roll_mean_3_month = rollmean(x = arithmetic_mean, k = 3, align = "right", fill = NA_real_)) %>%
  ungroup() %>%
  group_by(aqs_sitecode, parameter, year) %>%
  slice_max(roll_mean_3_month) %>%
  ungroup() %>%
  select(aqs_sitecode, year, parameter, roll_mean_3_month_max = roll_mean_3_month) %>%
  distinct()

lead_dvs <- daily_lead %>%
  filter(event_type %in% c("No Events", "Concurred Events Excluded")) %>%
  select(aqs_sitecode, date_local, year, parameter, poc, method, arithmetic_mean) %>%
  mutate(month = as.numeric(format(date_local, "%m"))) %>%
  group_by(aqs_sitecode, year, month, parameter, method, poc) %>%
  reframe(arithmetic_mean = mean(arithmetic_mean, na.rm = T)) %>%
  ungroup() %>%
  arrange(aqs_sitecode, parameter, method, poc, year, month) %>%
  group_by(aqs_sitecode, parameter, method, poc) %>%
  mutate(roll_mean_3_month = rollmean(x = arithmetic_mean, k = 3, align = "right", fill = NA_real_)) %>%
  ungroup()

groups <- lead_dvs %>%
  select(aqs_sitecode, parameter, method, poc) %>%
  distinct() %>%
  mutate(id = row_number())

dvs <- data.frame()
for (i in 1:nrow(groups)) {
  df <- lead_dvs %>%
    filter(aqs_sitecode == groups[i,]$aqs_sitecode, parameter == groups[i,]$parameter, method == groups[i,]$method, poc == groups[i,]$poc)
  df_temp <- data.frame()
  years <- unique(df$year)
  for (y in years) {
    if (y %in% c(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y")) - 1)) {
      next
    } else {
      dv_years <- c(y, y + 1, y + 2)
      df_filt <- df %>%
        filter(year %in% dv_years) %>%
        mutate(dv = round(max(roll_mean_3_month, na.rm = T), digits = 2),
               month_count = sum(!is.na(roll_mean_3_month)),
               dv_period = paste0(as.character(dv_years[1]), "-", as.character(dv_years[3]))) %>%
        select(-month, -arithmetic_mean, -roll_mean_3_month, -year) %>%
        distinct()
      df_temp <- rbind(df_temp, df_filt)
    }
  }
  dvs <- rbind(dvs, df_temp)
  print(i)
}
rm(df, i, y, dv_years, df_filt, df_temp, groups, lead_dvs)
gc()

#### carbon monoxide ####
# requirement: 1-hour co data downloaded from AQS
co <- data.frame()
for (s in unique(sample_data$site)) {
  data_temp <- sample_data %>%
    filter(parameter == "Carbon monoxide", site == s) %>%
    arrange(datetime_local) %>%
    rename(date = datetime_local)
  df <- openair::rollingMean(mydata = data_temp, pollutant = "sample_measurement", width = 8, new.name = "sample_measurement_8hr", data.thresh = 75, align = "left")
  df <- df %>% distinct(date, .keep_all = TRUE)
  co <- rbind(co, df)
  rm(df, data_temp)
}
co <- co %>% filter(year %in% c(2021, 2022))
co_top50 <- co %>%
  group_by(site, year, poc) %>%
  arrange(-sample_measurement_8hr, .by_group = TRUE) %>%
  slice(1:50) %>% # find top 50 8 hour concentrations
  ungroup() %>%
  mutate(beg_interval = date - lubridate::hours(8), # 8 hour averaging right-aligned, so previous 8 hours go into the average
         time_avg_interval = lubridate::interval(beg_interval, date)) # create time interval column

co_8hr_max <- data.frame()
for (y in 2021:2022) {
  for (s in unique(co_top50$site)) {
    data_temp <- co_top50 %>%
      filter(year == y, site == s) %>%
      distinct(date, .keep_all = TRUE)
    if (nrow(data_temp) == 0) {next} # nrow = 0 indicates there is no CO data for this site for this year
    max_8hour_beg <- data_temp %>% # find the beginning interval for the max 8-hour average
      filter(sample_measurement_8hr == max(sample_measurement_8hr, na.rm = TRUE)) %>% 
      pull(beg_interval)
    max_8hour_end <- data_temp %>% # find ending interval for the max 8-hour average 
      filter(sample_measurement_8hr == max(sample_measurement_8hr, na.rm = TRUE)) %>% 
      pull(date)
    if (length(max_8hour_beg) > 1 & length(max_8hour_end) > 1) {    # handle ties in the max 8-hour averages
      max_8hour_beg_min <- max_8hour_beg[1]                         # first tie
      max_8hour_beg_max <- max_8hour_beg_min + lubridate::hours(8)  # eight hours later
      max_8hour_end_min <- max_8hour_end[1]                         # second tie
      max_8hour_end_max <- max_8hour_end_min + lubridate::hours(8)  # eight hours later
      max_hour <- data_temp %>%
        filter(between(date, max_8hour_beg_min, max_8hour_beg_max) | between(date, max_8hour_end_min, max_8hour_end_max)) %>% # filter between tie intervals
        arrange(-sample_measurement_8hr)
      max_hour <- max_hour[1:2,] 
      max_hour <- max_hour %>% pull(date)
      rm(max_8hour_beg_min, max_8hour_beg_max, max_8hour_end_min, max_8hour_end_max)
    } else if (length(max_8hour_beg) > 2 & length(max_8hour_end) > 2) {
      print ("Problem! More than two ties.")
      next
    } else {
      max_hour <- data_temp %>% # find date time of max 1-hour value that falls within the beginning and ending interval for the max 8-hour average
        filter(between(date, max_8hour_beg, max_8hour_end)) %>%
        slice(which.max(sample_measurement)) %>%
        pull(date)
      rm(max_8hour_beg, max_8hour_end)
    }
    if (length(max_hour) > 1) { # handle ties in the max 1-hour
      min_hour <- max_hour[1]
      max_hour <- max_hour[length(max_hour)]
      data_slice <- data_temp %>%
        slice(2:50) %>%
        mutate(overlap = ifelse(min_hour %within% time_avg_interval | max_hour %within% time_avg_interval, "TRUE", "FALSE")) %>%
        filter(overlap == "FALSE") %>%
        slice(which.max(sample_measurement_8hr))
      rm(min_hour, max_hour)
    } else if (length(max_hour) == 1) {
      data_slice <- data_temp %>%
        slice(2:50) %>%
        mutate(overlap = max_hour %within% time_avg_interval) %>% # if the 2-50th ranked 8-hour averages have averaging intervals that contain the date time of the max 1-hour value, assign TRUE (e.g., not a candidate for second-highest max)
        filter(overlap == FALSE) %>%
        slice(which.max(sample_measurement_8hr)) # find highest 8-hour average
      rm(max_hour)
    }
    data_slice <- data_slice %>% select(-overlap)
    co_8hr_max <- rbind(co_8hr_max, data_slice)
  }
}
rm(data_slice, data_temp)