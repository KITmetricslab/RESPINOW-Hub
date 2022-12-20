Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub/code/baseline")
library(zoo)
source("functions.R")

plot_all <- FALSE

path_data <- "/home/johannes/Documents/RESPINOW/RESPINOW-Data"


labels_diseases <- c("seasonal_influenza" = "Seasonal influenza",
                     "rsv_infection" = "RSV",
                     "pneumococcal_disease" = "Pneumococcal disease")
diseases <- names(labels_diseases)

# dates for which to produce nowcasts:
forecast_dates <- seq(from = as.Date("2022-12-18"),
                      to = as.Date("2022-12-18"),
                      by = 7)

# forecast_dates <- as.Date("2022-12-04")

# limited by number of observations (in the early part, not relevant anymore)
n_history_dispersion <- 15
n_history_expectations <- 15
max_delay <- 10
max_horizon <- 4

# read in data:
triangles <- list()
for (disease in diseases) {
  triangles[[disease]] <- read.csv(paste0(path_data, "/data/Survstat/", disease, 
                                          "_reporting_triangle_survstat_preprocessed.csv"),
                                   colClasses = c("date" = "Date"))
}



for(i in seq_along(forecast_dates)){
  all_nc <- NULL
  
  forecast_date <- forecast_dates[i]
  cat(as.character(forecast_dates[i]), "\n")
  
  for (disease in diseases) {
    # generate nowcasts for age groups ("00+" is covered in loop over locations)
    for(ag in c("00-04", "05-14", "15-34", "35-59", "60-79", "80+")){
      
      observed_temp <- subset(triangles[[disease]], location == "DE" & age_group == ag)
      # generate truth data as of forecast_date
      observed_temp_back_in_time <- back_in_time_df(observed_temp, date = forecast_date)
      
      # prepare for plotting:
      plot_data <- data.frame(date = observed_temp$date,
                              value = rowSums(observed_temp[, grepl("value_", colnames(observed_temp))],
                                              na.rm = TRUE))
      
      plot_data_back_in_time <- data.frame(date = as.Date(observed_temp_back_in_time$date),
                                           value = rowSums(observed_temp_back_in_time[, grepl("value_", colnames(observed_temp_back_in_time))], 
                                                           na.rm = TRUE))
      
      # compute nowcast:
      nc <- compute_nowcast(observed = observed_temp_back_in_time, 
                            location = "DE", 
                            age_group = ag, 
                            n_history_expectations = n_history_expectations, 
                            n_history_dispersion = n_history_dispersion,
                            min_horizon = 0,
                            max_horizon = max_horizon,
                            max_delay = max_delay,
                            pathogen = disease,
                            target_type = "inc case")
      
      # generate a plot:
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
      
      if(is.null(all_nc)){
        all_nc <- nc
      }else{
        all_nc <- rbind(all_nc, nc)
      }
    }
    
    # generate nowcasts for federal states:
    locations <- sort(unique(triangles[[disease]]$location))
    
    for(loc in locations){
      
      observed_temp <- subset(triangles[[disease]], location == loc & age_group == "00+")
      # generate truth data as of forecast_date
      observed_temp_back_in_time <- back_in_time_df(observed_temp, date = forecast_date)
      
      # prepare for plotting:
      plot_data <- data.frame(date = observed_temp$date,
                              value = rowSums(observed_temp[, grepl("value_", colnames(observed_temp))],
                                              na.rm = TRUE))
      
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
      
      # generate a plot:
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
      
      if(is.null(all_nc)){
        all_nc <- nc
      }else{
        all_nc <- rbind(all_nc, nc)
      }
    }
  }
  
  # write out:
  if(forecast_date == Sys.Date()){
    write.csv(all_nc, file = paste0("../../data-processed/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }else{
    cat("forecast_date is in the past, writing to data-processed_retrospective \n")
    write.csv(all_nc, file = paste0("../../data-processed_retrospective/KIT-simple_nowcast/", forecast_date, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
}



