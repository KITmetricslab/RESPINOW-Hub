# Apply the KIT-simple_nowcast baseline model to NRZ data.
# Author: Johannes Bracher, johannes.bracher@kit.edu

# Some code which is only needed when running this file individually
run_individually <- FALSE
if(run_individually){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")
  
  setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub/code/baseline")
  # library used for rolling sums:
  library(zoo)
  # get functions:
  source("functions.R")
  
  # should nowcasts be plotted for all strata?
  plot_all <- FALSE
  
  # path where the data repository can be found:
  path_data <- "/home/johannes/Documents/RESPINOW/RESPINOW-Data"
  
  # dates for which to produce nowcasts:
  forecast_dates <- seq(from = as.Date("2023-01-22"),
                        to = as.Date("2023-01-22"),
                        by = 7)
}


# names of diseases in the respective data source:
diseases <- c("influenza", "rsv")


# set the sizes of training data sets
# limited by number of observations
n_history_dispersion <- 8
n_history_expectations <- 8
max_delay <- 5
max_horizon <- 4
weeks_to_exclude <- c(52) # Christmas weeks should be excluded from fittin due to 0 initial reporting

# read in data:
triangles <- list()
for (disease in diseases) {
  triangles[[disease]] <- read.csv(paste0(path_data, "/data/NRZ/", disease, 
                                          "_reporting_triangle_NRZ_preprocessed.csv"),
                                   colClasses = c("date" = "Date"))
  # remove stray column:
  triangles[[disease]]$value_.10w <- NULL
}


# run over forecast dates to generate nowcasts:
for(i in seq_along(forecast_dates)){
  forecast_date <- forecast_dates[i]
  cat(as.character(forecast_dates[i]), "\n")
  
  # a place holder for a data frame in which nowcasts will be stored 
  all_nc <- NULL
  
  # run over diseases:
  for (disease in diseases) {
    # generate nowcasts for federal states:
    locations <- sort(unique(triangles[[disease]]$location))
    
    for(loc in locations){
      # subset to relevant data:
      observed_temp <- subset(triangles[[disease]], location == loc & age_group == "00+")
      # generate truth data as of forecast_date:
      observed_temp_back_in_time <- back_in_time_df(observed_temp, date = forecast_date)
      
      # prepare for plotting:
      # latest values (red line in plot):
      plot_data <- data.frame(date = observed_temp$date,
                              value = rowSums(observed_temp[, grepl("value_", colnames(observed_temp))],
                                              na.rm = TRUE))
      
      # values as in real time (black line in plot)
      plot_data_back_in_time <- data.frame(date = as.Date(observed_temp_back_in_time$date),
                                           value = rowSums(observed_temp_back_in_time[, grepl("value_", colnames(observed_temp_back_in_time))], 
                                                           na.rm = TRUE))
      
      # compute nowcast:
      nc <- compute_nowcast(observed = observed_temp_back_in_time, 
                            location = loc, 
                            age_group = "00+",
                            n_history_expectations = n_history_expectations,
                            n_history_dispersion = n_history_dispersion,
                            min_horizon = 0,
                            max_horizon = max_horizon,
                            max_delay = max_delay,
                            pathogen = disease,
                            target_type = "inc case")
      
      # generate a plot if desired:
      if(plot_all | loc == "DE"){
        plot_forecast(forecasts = nc,
                      location = loc, age_group = "00+",
                      truth = plot_data_back_in_time, target_type = paste("inc case"),
                      levels_coverage = c(0.5, 0.95),
                      start = as.Date(forecast_date) - 135,
                      end = as.Date(forecast_date) + 28,
                      forecast_date = forecast_date, 
                      ylim = c(0, 1.2*max(tail(plot_data_back_in_time$value, 20)))
        )
        lines(plot_data$date, plot_data$value, col = "red", lty  ="solid")
        title(paste0(disease, ",", loc, ", ", forecast_date))
      }
      
      # add to all_nc
      if(is.null(all_nc)){
        all_nc <- nc
      }else{
        all_nc <- rbind(all_nc, nc)
      }
    }
  }
  
  # add name of data source:
  all_nc$data_source <- "nrz"
  
  # write out if this file is used individually (specify at top).
  # No writing out if used through_run_baseline_all
  if(run_individually){
    if(forecast_date == Sys.Date()){
      write.csv(all_nc, file = paste0("../../data-processed/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
    }else{
      cat("forecast_date is in the past, writing to data-processed_retrospective \n")
      write.csv(all_nc, file = paste0("../../data-processed_retrospective/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
    }
  }
}



