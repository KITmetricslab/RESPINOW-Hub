# Run baseline model for all data sources.
# Each one is treated in a separate script due to different particularities of
# the data sources.
# Author: Johannes Bracher, johannes.bracher@kit.edu

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub/code/baseline")
# library used for rolling sums:
library(zoo)
# get functions:
source("functions.R")

# should nowcasts be plotted for all strata?
plot_all <- FALSE

# path to repo with plot data:
path_data <- "/home/johannes/Documents/RESPINOW/RESPINOW-Data"

# dates for which nowcasts are to be produced:
all_forecast_dates <- seq(from = as.Date("2023-01-22"),
                      to = as.Date("2023-01-22"),
                      by = 7)

if(any(weekdays(all_forecast_dates) != "Sunday")){
  warning("Forecast dates need to be Sundays.")
}

# run over forecast dates to generate nowcasts:
for(i in seq_along(all_forecast_dates)){
  # select forecast_date.
  # note: inside the different files, a vector of forecast dates can be used,
  # (thus the denomination forecast_dates) but we only pass one of them at a 
  # time when using this wrap-up script
  forecast_dates <- all_forecast_dates[i]
  
  cat(as.character(forecast_dates[i]), "\n")
  
  # a place holder for a data frame in which nowcasts will be stored 
  all_all_nc <- NULL
  # note: in each file an object all_nc is generated and these are appended
  # in all_all_nc
  
  # run baseline for SurvStat
  source("run_baseline_survstat.R")
  
  # add to all_all_nc
  if(is.null(all_nc)){
    all_all_nc <- all_nc
  }else{
    all_all_nc <- rbind(all_all_nc, all_nc)
  }
  # remove all_nc and some other objects to avoid accidentially re-using them
  rm(all_nc, diseases, n_history_dispersion, n_history_expectations, max_delay, max_horizon)
  
  
  # run baseline for NRZ
  # source("run_baseline_NRZ.R")

  # # add to all_all_nc
  # if(is.null(all_nc)){
  #   all_all_nc <- all_nc
  # }else{
  #   all_all_nc <- rbind(all_all_nc, all_nc)
  # }
  # # remove all_nc and some other objects to avoid accidentially re-using them
  # rm(all_nc, diseases, n_history_dispersion, n_history_expectations, max_delay, max_horizon)

  # write out:
  if(forecast_date == Sys.Date()){
    write.csv(all_nc, file = paste0("../../data-processed/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }else{
    cat("forecast_date is in the past, writing to data-processed_retrospective \n")
    write.csv(all_all_nc, file = paste0("../../data-processed_retrospective/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
}
