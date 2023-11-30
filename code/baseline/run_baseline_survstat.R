# Apply the KIT-simple_nowcast baseline model to SurvStat data.
# Author: Johannes Bracher, johannes.bracher@kit.edu

# Some code which is only needed when running this file individually
run_individually <- TRUE
if(run_individually){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")
  
  # path of the repo:
  path_repo <- "/home/johannes/Documents/RESPINOW/RESPINOW-Hub"
  
  # library used for rolling sums:
  library(zoo)
  # get functions:
  source(paste0(path_repo, "/code/baseline/functions.R"))
  source(paste0(path_repo, "/respinow_viz/functions.R"))
  
  # should nowcasts be plotted for all strata?
  plot_all <- TRUE
}

# define data source:
data_source <- "icosari"

# the diseases present in the data source:
diseases <- list.dirs(paste0(path_repo, "/data/", data_source), recursive = FALSE, full.names = FALSE)
# diseases <- "sari"

# dates for which to produce nowcasts:
forecast_dates <- seq(from = as.Date("2023-11-16"),
                      to = as.Date("2023-11-16"),
                      by = 7)

# set the sizes of training data sets
# limited by number of observations (in the early part, not relevant anymore)
n_history_dispersion <- 15
n_history_expectations <- 15
max_delay <- 4
max_horizon <- 4

# read in data:
triangles <- list()
for (disease in diseases) {
  triangles[[disease]] <- read.csv(paste0(path_repo, "/data/", data_source, "/", disease, "/",
                                          "reporting_triangle-", data_source, "-", disease, "-preprocessed.csv"),
                                   colClasses = c("date" = "Date"), check.names = FALSE)
}


# run over forecast dates to generate nowcasts:
for(i in seq_along(forecast_dates)){
  forecast_date <- forecast_dates[i]
  cat(as.character(forecast_dates[i]), "\n")
  
  
  # run over diseases:
  for (disease in diseases) {
    
    # a place holder for a data frame in which nowcasts will be stored 
    all_nc <- NULL
    
    # generate nowcasts for age groups ("00+" is covered in loop over locations)
    # identify age groups:
    ags <- character(0) # sort(unique(triangles[[disease]]$age_group))
    for(ag in ags){
      
      # subsetting to relevant data:
      observed_temp <- subset(triangles[[disease]], location == "DE" & age_group == ag)
      # generate truth data as of forecast_date
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
      debug(compute_nowcast)
      nc <- compute_nowcast(observed = observed_temp_back_in_time, 
                            location = "DE", 
                            age_group = ag, 
                            forecast_date = forecast_date,
                            n_history_expectations = n_history_expectations, 
                            n_history_dispersion = n_history_dispersion,
                            min_horizon = 0,
                            max_horizon = max_horizon,
                            max_delay = max_delay,
                            pathogen = disease,
                            target_type = "inc case")
      
      # generate a plot if desired:
      # note: plot does currently not take max_lag into account
      if(plot_all){
        plot_forecast(forecasts = nc,
                      location = "DE", age_group = ag,
                      truth = plot_data_back_in_time, target_type = paste("inc case"),
                      levels_coverage = c(0.5, 0.95),
                      start = as.Date(forecast_date) - 135,
                      end = as.Date(forecast_date) + 28,
                      forecast_date = forecast_date, 
                      ylim = c(0, 1.2*max(tail(plot_data_back_in_time$value, 20)))
        )
        lines(plot_data$date, plot_data$value, col = "red", lty  ="solid")
        title(paste0(disease, ", ", ag, ", ", forecast_date))
      }
      
      # store in all_nc:
      if(is.null(all_nc)){
        all_nc <- nc
      }else{
        all_nc <- rbind(all_nc, nc)
      }
    }
    
    # generate nowcasts for federal states:
    # identify locations:
    locations <- "DE" # sort(unique(triangles[[disease]]$location))
    
    # run through locations:
    for(loc in locations){
      # subset to relevant data
      observed_temp <- subset(triangles[[disease]], location == loc & age_group == "00+")
      # generate truth data as of forecast_date
      observed_temp_back_in_time <- back_in_time_df(dat_truth = observed_temp, date = forecast_date,
                                                location = loc, age_group = "00+", max_lag = max_delay)
      
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
      undebug(compute_nowcast)
      nc <- compute_nowcast(observed = observed_temp_back_in_time, 
                            location = loc, 
                            age_group = "00+",
                            forecast_date = forecast_date,
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
      
      # store in all_nc
      if(is.null(all_nc)){
        all_nc <- nc
      }else{
        all_nc <- rbind(all_nc, nc)
      }
    }
    
    # write out:
    write.csv(all_nc, file = paste0(path_repo, "/submissions/retrospective/", data_source, "/", disease, "/KIT-simple_nowcast/",
                                    forecast_date, "-", data_source, "-", disease, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
  
  # all_nc$data_source <- "survstat"
  

  
  # # No writing out if used through_run_baseline_all
  # if(run_individually){
  #   if(forecast_date == Sys.Date()){
  #     write.csv(all_nc, file = paste0("../../data-processed/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  #   }else{
  #     cat("forecast_date is in the past, writing to data-processed_retrospective \n")
  #     write.csv(all_nc, file = paste0("../../data-processed_retrospective/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  #   }
  # }
}



