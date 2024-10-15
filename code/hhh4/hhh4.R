# Apply the KIT-simple_nowcast baseline model to national-level unstratified data.
# Author: Johannes Bracher, johannes.bracher@kit.edu

install.packages(c("surveillance"))
# install.packages("devtools", dependencies = TRUE)
# library("devtools")

GITHUB_PAT = Sys.getenv("PAT_DW")

install.packages("remotes")
remotes::install_github("jbracher/hhh4addon", auth_token=GITHUB_PAT)
                 
library(surveillance)
library(hhh4addon)

# Some code which is only needed when running this file individually
run_individually <- TRUE
if(run_individually){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

  # path of the repo:
  path_repo <- "."

  # library used for rolling sums:
  library(zoo)
  # get functions:
  source(paste0(path_repo, "/code/baseline/functions_general.R"))
  source(paste0(path_repo, "/respinow_viz/functions.R"))

  # should nowcasts be plotted for all strata?
  plot_all <- FALSE
}

# define data source:
data_sources <- c("icosari", "agi")

all_diseases <- list("icosari" = c("sari"),
                     "agi" = c("are"))

types <- list("icosari" = "additions",
              "agi" = "revise_average")


# define a period of time not used for fitting
exclusion_period <- seq(from = as.Date("2019-06-30"), to = as.Date("2023-07-03"), by = 1)

# dates for which to produce nowcasts:
# forecast_dates <- seq(from = as.Date("2024-07-04"),
#                      to = as.Date("2024-10-03"),
#                      by = 7)
# Select most recent Thursday as forecast_date:
forecast_dates0 <- Sys.Date() - 0:6
forecast_dates <- forecast_dates0[weekdays(forecast_dates0) == "Thursday"]

# set the sizes of training data sets
# limited by number of observations (in the early part, not relevant anymore)
n_history_dispersion <- 15
n_history_expectations <- 15
max_delay <- 4
max_horizon <- 4 # ifelse(discard_last_value, 5, 4)
quantile_levels_nowcast <- 1:99/100

# define the control list:
ctrl <- list(end = list(f = addSeason2formula(~ 1, period = 52.25)),
             ar = list(f = addSeason2formula(~ 1)),
             family = "NegBin1",
             par_lag = 0.5) # this is a pragmatic choice for now; roughly 0.62 on first lag

# run through data sources:

for(data_source in data_sources){
  
  diseases <- all_diseases[[data_source]]
  
  # read in data:
  
  # reporting triangles:
  triangles <- list()
  for (disease in diseases) {
    triangles[[disease]] <- read.csv(paste0(path_repo, "/data/", data_source, "/", disease, "/",
                                            "reporting_triangle-", data_source, "-", disease, "-preprocessed.csv"),
                                     colClasses = c("date" = "Date"), check.names = FALSE)
  }
  
  # time series which reach back further
  timeseries <- list()
  for (disease in diseases) {
    timeseries[[disease]] <- read.csv(paste0(path_repo, "/data/", data_source, "/", disease, "/",
                                             "latest_data-", data_source, "-", disease, ".csv"),
                                      colClasses = c("date" = "Date"), check.names = FALSE)
  }
  
  # run over forecast dates to generate nowcasts:
  for(i in seq_along(forecast_dates)){
    forecast_date <- forecast_dates[i]
    cat(as.character(forecast_dates[i]), "\n")
    
    
    # run over diseases:
    for (disease in diseases) {
      
      # a place holder for a data frame in which nowcasts will be stored
      all_fc <- NULL
      
      # generate nowcasts for federal states:
      # identify locations:
      locations <- "DE" # sort(unique(triangles[[disease]]$location))
      
      # run through locations:
      for(loc in locations){
        
        # prepare for plotting:
        # truth data as of forecast_date, subset to relevant stratum
        reporting_triangle_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,
                                                      location = loc, age_group = "00+", max_lag = max_delay)
        plot_data_back_in_time <- data.frame(date = as.Date(reporting_triangle_back_in_time$date),
                                             value = rowSums(reporting_triangle_back_in_time[, grepl("value_", colnames(reporting_triangle_back_in_time))],
                                                             na.rm = TRUE))
        
        # current version of truth data, subset to relevant stratum
        reporting_triangle_current <- data_as_of(dat_truth = triangles[[disease]], date = Sys.Date(),
                                                 location = loc, age_group = "00+", max_lag = max_delay)
        plot_data_current <- data.frame(date = reporting_triangle_current$date,
                                        value = rowSums(reporting_triangle_current[, grepl("value_", colnames(reporting_triangle_current))],
                                                        na.rm = TRUE))
        
        # compute nowcast:
        nc <- compute_nowcast(observed = triangles[[disease]],
                              location = loc,
                              age_group = "00+",
                              type = types[[data_source]],
                              forecast_date = forecast_date,
                              n_history_expectations = n_history_expectations,
                              n_history_dispersion = n_history_dispersion,
                              max_delay = max_delay,
                              quantile_levels = quantile_levels_nowcast)
        nc <- nc$result
        
        # get time series, restrict to location
        ts <- subset(timeseries[[disease]], location == loc & age_group == "00+" & date <= max(reporting_triangle_back_in_time$date))
        # remove exclusion period
        ts$value[ts$date %in% exclusion_period] <- NA
        
        # initialize matrices to store mu and var:
        mu_matrix <- var_matrix <- matrix(nrow = max_horizon, ncol = length(quantile_levels_nowcast))
        
        # wrap into function from here on
        # replace last weeks with nowcasts and generate forecasts:
        for (j in seq_along(quantile_levels_nowcast)) {
          nc_temp <- subset(nc, quantile == quantile_levels_nowcast[j])
          dates_to_replace <- sort(nc_temp$target_end_date)
          values_to_replace <- nc_temp$value[order(nc_temp$target_end_date)]
          # plug these nowcasts into the time series
          ts_temp <- ts
          ts_temp$value[ts_temp$date %in% dates_to_replace] <- values_to_replace
          
          # generate forecasts:
          sts_temp <- sts(c(round(ts_temp$value), rep(NA, max_horizon)), start = c(ts_temp$year[1], ts_temp$week[1]))
          ctrl$subset <- 6:nrow(ts_temp)
          fit_temp <- hhh4_lag(sts_temp, control = ctrl)
          # 5 weeks if last value discarded, 4 otherwise
          forecast_temp <- predictive_moments(fit_temp, t_condition = nrow(ts_temp),
                                              lgt = max_horizon)
          mu_matrix[, j] <- forecast_temp$mu_matrix[, 1]
          var_matrix[, j] <- forecast_temp$var_matrix[, 1]
          # plot(fit_temp)
          # fanplot_prediction(forecast_temp, add = TRUE)
        }
        
        dates_forecast <- seq(from = max(ts_temp$date + 7), by = 7, length.out = max_horizon)
        pred_means <- rowMeans(mu_matrix)
        
        # get quantiles via a negative binomial approximation based on moments, format and store:
        support <- seq(from = 0, to = 3*max(mu_matrix), by = 100)
        quantile_matrix <- matrix(nrow = 4, ncol = 7)
        quantile_levels <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
        fc <- NULL
        for(h in 1:4){
          # a matrix to store CDFs
          cdf_matrix <- matrix(NA, ncol = length(quantile_levels_nowcast), nrow = length(support))
          for(k in 1:ncol(cdf_matrix)){
            # extract moments from matrices where they are stored
            mu <- mu_matrix[h, k]
            sigma2 <- var_matrix[h, k]
            size <- pmin(abs(mu / (sigma2 / mu - 1)), 10000)
            # fill in CDF:
            cdf_matrix[, k] <- pnbinom(support, mu = mu, size = size)
          }
          # aggregate CDFs:
          cdf <- rowMeans(cdf_matrix)
          
          # extract quantiles:
          for(k in seq_along(quantile_levels)){
            quantile_matrix[h, k] <- max(support[cdf <= quantile_levels[k]])
          }
          
          # target end dates:
          t_e_d <- max(ts_temp$date) + 7*h
          n_quantile_levels <- length(quantile_levels)
          # fill data.frame with quantiles:
          quantiles_formatted <- data.frame(location = rep(loc, n_quantile_levels),
                                            age_group = rep("00+", n_quantile_levels),
                                            forecast_date = rep(forecast_date, n_quantile_levels),
                                            target_end_date = rep(t_e_d, each = n_quantile_levels),
                                            horizon = rep(h, each = n_quantile_levels),
                                            type = "quantile",
                                            quantile = quantile_levels,
                                            value = quantile_matrix[h, ])
          # add mean:
          mean_formatted <- quantiles_formatted[1, ]
          mean_formatted$type <- "mean"
          mean_formatted$quantile <- NA
          mean_formatted$value <- mean(mu_matrix[h, ])
          
          # append:
          all_formatted <- rbind(quantiles_formatted, mean_formatted)
          
          if(is.null(fc)){
            fc <- all_formatted
          }else{
            fc <- rbind(fc, all_formatted)
          }
        }
        
        # generate a plot if desired:
        # if(plot_all | loc == "DE"){
        #   plot_forecast(forecasts = fc,
        #                 location = loc, age_group = "00+",
        #                 truth = plot_data_back_in_time,
        #                 levels_coverage = c(0.5, 0.95),
        #                 start = as.Date("2023-08-31"),
        #                 # end = as.Date("2024-02-28"),
        #                 forecast_date = forecast_date, 
        #                 # ylim = c(0, 2*max(tail(plot_data_back_in_time$value, 10)))
        #                 ylim = c(0,50000)
        #   )
        #   # lines(dates_forecast, pred_means, col = "blue")
        #   # lines(dates_forecast, quantile_matrix[, 2], col = "blue", lty = 2)
        #   # lines(dates_forecast, quantile_matrix[, 6], col = "blue", lty = 2)
        #   lines(plot_data_current$date, plot_data_current$value, col = "red", lty  ="solid")
        #   title(paste0(disease, ",", loc, ", ", forecast_date))
        # }
        
        # store in all_fc
        if(is.null(all_fc)){
          all_fc <- fc
        }else{
          all_fc <- rbind(all_fc, fc)
        }
      }
      
      # write out:
      write.csv(all_fc, file = paste0(path_repo, "/submissions/", data_source, "/", disease, "/KIT-hhh4/",
                                      forecast_date, "-", data_source, "-", disease, "-KIT-hhh4.csv"), row.names = FALSE)
    }
  }
}




