# Apply the KIT-simple_nowcast baseline model to age-stratified data.
# Author: Johannes Bracher, johannes.bracher@kit.edu

# setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub")

# Some code which is only needed when running this file individually
run_individually <- TRUE
if(run_individually){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")
  
  # path of the repo:
  path_repo <- "."
  
  # get functions:
  source(paste0(path_repo, "/code/baseline/functions_general.R"))
  source(paste0(path_repo, "/respinow_viz/functions.R"))
  
  # should nowcasts be plotted for all strata?
  plot_all <- FALSE
}

# define data sources:
data_sources <- c("icosari", "survstat", "agi")

# diseases per data source.
all_diseases <- list("icosari" = c("sari"),
                     "survstat" = c("influenza"),
                     "agi" = c("are"))

# the type of nowcast correction which is necessary:
types <- list("icosari" = "additions",
              "survstat" = "additions",
              "agi" = "revise_average")

# how borrowing of delays is done:
borrow_delays <- list("icosari" = TRUE,
                      "survstat" = TRUE,
                      "agi" = TRUE)

# how borrowing of dispersion parameters is done
borrow_dispersion <- list("icosari" = TRUE,
                          "survstat" = TRUE,
                          "agi" = FALSE)
# size parameters can be shared well, for sd this does not make sense.

# dates for which to produce nowcasts:
# for retrospective generation:
# forecast_dates <- seq(from = as.Date("2023-12-07"),
#                       to = as.Date("2024-10-03"),
#                       by = 7)
# Select most recent Thursday as forecast_date:
forecast_dates0 <- Sys.Date() - 0:6
forecast_dates <- forecast_dates0[weekdays(forecast_dates0) == "Thursday"]

# set the sizes of training data sets
# limited by number of observations (in the early part, not relevant anymore)
n_history_dispersion <- 15
n_history_expectations <- 15
max_delay <- 4
max_horizon <- 3

# run through data sources:
for (data_source in data_sources) {
  # get diseases:
  diseases <- all_diseases[[data_source]]
  
  # read in reporting triangles:
  triangles <- list()
  for (disease in diseases) {
    triangles[[disease]] <- read.csv(paste0(path_repo, "/data/", data_source, "/", disease, "/",
                                            "reporting_triangle-", data_source, "-", disease, ".csv"),
                                     colClasses = c("date" = "Date"), check.names = FALSE)
  }
  # note: we load raw RTs, preprocessing takes place inside compute_nowcast where appropriate
  
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
      ags <- sort(unique(triangles[[disease]]$age_group))
      ags <- ags[ags != "00+"]
      for(ag in ags){
        
        # prepare for plotting:
        # truth data as of forecast_date, subset to relevant stratum
        observed_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,
                                            location = "DE", age_group = ag, max_lag = max_delay)
        plot_data_back_in_time <- data.frame(date = as.Date(observed_back_in_time$date),
                                             value = rowSums(observed_back_in_time[, grepl("value_", colnames(observed_back_in_time))],
                                                             na.rm = TRUE))
        
        # current version of truth data, subset to relevant stratum
        observed_current <- data_as_of(dat_truth = triangles[[disease]], date = Sys.Date(),
                                       location = "DE", age_group = ag, max_lag = max_delay)
        plot_data_current <- data.frame(date = observed_current$date,
                                        value = rowSums(observed_current[, grepl("value_", colnames(observed_current))],
                                                        na.rm = TRUE))
        
        # compute nowcast.
        # note: pre-processing and subsetting of RT takes place inside
        nc <- compute_nowcast(observed = triangles[[disease]],
                              location = "DE",
                              age_group = ag,
                              forecast_date = forecast_date,
                              observed2 = triangles[[disease]],
                              location2 = "DE",
                              age_group2 = "00+",
                              type = types[[data_source]],
                              borrow_delays = borrow_delays[[data_source]],
                              borrow_dispersion = borrow_dispersion[[data_source]],
                              n_history_expectations = n_history_expectations,
                              n_history_dispersion = n_history_dispersion,
                              max_delay = max_delay)
        nc <- nc$result
        
        # generate a plot if desired:
        # note: plot does currently not take max_lag into account
        if(plot_all){
          plot_forecast(forecasts = nc,
                        location = "DE", age_group = ag,
                        truth = plot_data_back_in_time,
                        levels_coverage = c(0.5, 0.95),
                        start = as.Date(forecast_date) - 135,
                        end = as.Date(forecast_date) + 28,
                        forecast_date = forecast_date,
                        ylim = c(0, 1.2*max(tail(plot_data_back_in_time$value, 20)))
          )
          lines(plot_data_current$date, plot_data_current$value, col = "red", lty  ="solid")
          title(paste0(disease, ", ", ag, ", ", forecast_date))
        }
        
        # store in all_nc:
        if(is.null(all_nc)){
          all_nc <- nc
        }else{
          all_nc <- rbind(all_nc, nc)
        }
      }
      
      # generate nowcasts for different locations (DE and if available, regions):
      # identify locations:
      locations <- sort(unique(triangles[[disease]]$location))
      
      # run through locations:
      for(loc in locations){
        
        # prepare for plotting:
        # truth data as of forecast_date, subset to relevant stratum
        observed_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,
                                            location = loc, age_group = "00+", max_lag = max_delay)
        plot_data_back_in_time <- data.frame(date = as.Date(observed_back_in_time$date),
                                             value = rowSums(observed_back_in_time[, grepl("value_", colnames(observed_back_in_time))],
                                                             na.rm = TRUE))
        
        # current version of truth data, subset to relevant stratum
        observed_current <- data_as_of(dat_truth = triangles[[disease]], date = Sys.Date(),
                                       location = loc, age_group = "00+", max_lag = max_delay)
        plot_data_current <- data.frame(date = observed_current$date,
                                        value = rowSums(observed_current[, grepl("value_", colnames(observed_current))],
                                                        na.rm = TRUE))
        
        # compute nowcast:
        # note: pre-processing and subsetting of RT takes place inside
        nc <- compute_nowcast(observed = triangles[[disease]],
                              location = loc,
                              age_group = "00+",
                              observed2 = triangles[[disease]],
                              location2 = "DE",
                              age_group2 = "00+",
                              borrow_delays = borrow_delays[[data_source]],
                              borrow_dispersion = borrow_dispersion[[data_source]],
                              type = types[[data_source]],
                              forecast_date = forecast_date,
                              n_history_expectations = n_history_expectations,
                              n_history_dispersion = n_history_dispersion,
                              max_delay = max_delay)
        nc <- nc$result
        
        # generate a plot if desired:
        if(plot_all | loc == "DE"){
          plot_forecast(forecasts = nc,
                        location = loc, age_group = "00+",
                        truth = plot_data_back_in_time,
                        levels_coverage = c(0.5, 0.95),
                        start = as.Date(forecast_date) - 135,
                        end = as.Date(forecast_date) + 28,
                        forecast_date = forecast_date,
                        ylim = c(0, 2*max(tail(plot_data_back_in_time$value, 10)))
          )
          lines(plot_data_current$date, plot_data_current$value, col = "red", lty  ="solid")
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
      write.csv(all_nc, file = paste0(path_repo, "/submissions/", data_source, "/", disease, "/KIT-simple_nowcast/",
                                     forecast_date, "-", data_source, "-", disease, "-KIT-simple_nowcast.csv"), row.names = FALSE)
    }
  }
}