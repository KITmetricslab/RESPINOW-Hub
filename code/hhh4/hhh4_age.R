# Apply the KIT-simple_nowcast baseline model to stratified data
# Author: Johannes Bracher, johannes.bracher@kit.edu

# install.packages(c("zoo", "surveillance"))
# install.packages("devtools", dependencies = TRUE)
# library("devtools")

# GITHUB_PAT = Sys.getenv("PAT_DW")

# install.packages("remotes")
# remotes::install_github("jbracher/hhh4addon", auth_token=GITHUB_PAT)

setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub")

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
  source(paste0(path_repo, "/code/baseline/functions.R"))
  source(paste0(path_repo, "/respinow_viz/functions.R"))
  
  # should nowcasts be plotted for all strata?
  plot_all <- FALSE
}

# define data source:
# define data source:
data_sources <- c("icosari", "agi")

# the diseases present in the data sources:
all_diseases <- list("icosari" = c("sari"),
                     "agi" = c("are"))

# "type" argument for nowcasts
types <- list("icosari" = "additions",
              "agi" = "revise_average")

# define a period of time not used for fitting
exclusion_period <- seq(from = as.Date("2019-06-30"), to = as.Date("2023-07-03"), by = 1)

# contact matrices (currently not used):
# library(hhh4contacts)
# contacts0 <- hhh4contacts::contactmatrix(grouping = c(1, 2, 4, 5, 2, 1))
# contacts <- contacts0
# rownames(contacts) <- colnames(contacts) <- ags
# contacts[, "60-79"] <- contacts0[, "60-69"] + 0.5*contacts0[, "70+"]
# contacts[, "80+"] <- 0.5*contacts0[, "70+"]
# names(attr(contacts, "agedistri")) <- ags
# agedistri <- attr(contacts0, "agedistri")
# attr(contacts, "agedistri")["60-79"] <- agedistri["60-69"] + 0.5*agedistri["70+"]
# attr(contacts, "agedistri")["80+"] <- 0.5*agedistri["70+"]
# # reinforce diagonal
# diag(contacts) <- 1.5*diag(contacts)


# dates for which to produce nowcasts:
# Select most recent Thursday as forecast_date:
# forecast_dates0 <- Sys.Date() - 0:6
# forecast_dates <- forecast_dates0[weekdays(forecast_dates0) == "Thursday"]

forecast_dates <- seq(from = as.Date("2024-07-04"),
                      to = as.Date("2024-10-03"),
                      by = 7)


# set the sizes of training data sets
# limited by number of observations (in the early part, not relevant anymore)
n_history_dispersion <- 15
n_history_expectations <- 15
max_delay <- 4
max_horizon <- 4 # ifelse(discard_last_value, 5, 4)
quantile_levels_nowcast <- seq(from = 0.025, to = 0.975, by = 0.05)
quantile_levels <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

# define the control list (currently not used here, inside loop instead):
# ctrl <- list(# end = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
#              #                                  period = 52.25)),
#              end = list(f = ~ 0),
#              ne = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
#                                              period = 52.25),
#                        weights = contacts, normalized = TRUE),
#              family = "NegBinM",
#              par_lag = 0.5) # this is a pragmatic choice for now; roughly 0.62 on first lag
# 

# run through data sources:
for(data_source in data_sources){
  
  # get diseases
  diseases <- all_diseases[[data_source]]
  
  
  # read in reporting triangles:
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
  
  # run over forecast dates to generate forecasts:
  for(i in seq_along(forecast_dates)){
    
    forecast_date <- forecast_dates[i]
    cat(as.character(forecast_dates[i]), "\n")
    
    
    # run over diseases:
    for (disease in diseases) {
      
      # identif age groups:
      ags <- unique(triangles[[disease]]$age_group)
      ags <- ags[ags != "00+"]
      
      # a place holder for a data frame in which nowcasts will be stored
      all_nc <- all_fc <- NULL
      
      ###################################################
      # generate nowcasts for age groups:
      
      # run through age groups:
      for(ag in ags){
        # prepare for plotting:
        # truth data as of forecast_date, subset to relevant stratum
        reporting_triangle_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,
                                                      location = "DE", age_group = ag, max_lag = max_delay)
        
        
        # current version of truth data, subset to relevant stratum
        reporting_triangle_current <- data_as_of(dat_truth = triangles[[disease]], date = Sys.Date(),
                                                 location = "DE", age_group = ag, max_lag = max_delay)
        
        # compute nowcast:
        nc <- compute_nowcast(observed = triangles[[disease]],
                              location = "DE",
                              age_group = ag,
                              observed2 = triangles[[disease]],
                              location2 = "DE",
                              age_group2 = "00+",
                              type = types[[data_source]],
                              borrow_delays = TRUE,
                              borrow_dispersion = TRUE,
                              forecast_date = forecast_date,
                              n_history_expectations = n_history_expectations,
                              n_history_dispersion = n_history_dispersion,
                              max_delay = max_delay,
                              quantile_levels = quantile_levels_nowcast)
        nc <- nc$result
        
        # store / append:
        all_nc <- if(is.null(all_nc)){
          nc
        }else{
          rbind(all_nc, nc)
        }
      }
      
      ############################################################
      # forecasting based on nowcasts
      # get historical time series, remove 00+
      ts <- subset(timeseries[[disease]], location == "DE" & age_group != "00+" & 
                     date <= max(reporting_triangle_back_in_time$date))
      # remove exclusion period
      ts$value[ts$date %in% exclusion_period] <- NA
      
      # initialize matrices to store predictive means (mu) and variances (var):
      mu_array <- var_array <- array(dim = c(length(ags), 
                                             max_horizon, 
                                             length(quantile_levels_nowcast)),
                                     dimnames = list(ags, 
                                                     paste0("h", 1:max_horizon),
                                                     paste0("q", quantile_levels_nowcast)))
      # we will base NegBin approximations on these later.
      
      # temporary version of historical data into which we will plug nowcasts:
      ts_temp <- ts
      
      # replace last weeks with nowcasts and generate forecasts:
      for (j in seq_along(quantile_levels_nowcast)) {
        # for all age groups:
        for(ag in ags){
          nc_temp <- subset(all_nc, quantile == quantile_levels_nowcast[j] & age_group == ag)
          dates_to_replace <- sort(unique(nc_temp$target_end_date))
          values_to_replace <- nc_temp$value[order(nc_temp$target_end_date)]
          
          ts_temp$value[ts_temp$date %in% dates_to_replace & ts_temp$age_group == ag] <- values_to_replace
        }
        
        # set up a matrix for obsrvations incl. nowcast, to be passed to sts
        vect_00.04 <- subset(ts, age_group == "00-04")$value
        n_observed <- length(vect_00.04)
        matr_observed <- matrix(NA, ncol = length(ags), nrow = n_observed)
        colnames(matr_observed) <- ags
        for(i in seq_along(ags)){
          ag <- ags[i]
          matr_observed[, i] <- subset(ts_temp, age_group == ag)$value
        }
        # need to append NAs so predictive moments can be computed:
        matr_observed <- rbind(matr_observed, NA*tail(matr_observed, max_horizon))
        
        # generate forecasts:
        # cerate sts
        sts_temp <- sts(round(matr_observed), start = c(ts_temp$year[1], ts_temp$week[1]))

        # control list for hhh4
        ctrl <- list(end = # list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                       #                             period = 52.25)),
                       list(f = ~ 0),
                     ar = list(f = addSeason2formula(~0 + fe(1, unitSpecific = TRUE),
                                                     period = 52.25, S = rep(1, length(ags)))),
                     family = "NegBinM",
                     par_lag = 0.5,
                     subset = 6:nrow(sts_temp))
        
        # fit model
        fit_temp <- hhh4_lag(sts_temp, control = ctrl)
        # compute preditive moments: 5 weeks if last value discarded, 4 otherwise
        forecast_temp <- predictive_moments(fit_temp, t_condition = n_observed,
                                            lgt = max_horizon)
        # store these:
        mu_array[, , j] <- t(forecast_temp$mu_matrix)
        var_array[, , j] <- t(forecast_temp$var_matrix)
      }
      
      # get quantiles via negative binomial approximation, format and store:
      fc <- NULL
      # run over age groups:
      for(ag in ags){
        # extract predictive moments:
        mu_matrix <- mu_array[ag, , ]
        var_matrix <- var_array[ag, , ]
        pred_means <- rowMeans(mu_matrix)
        
        support <- seq(from = 0, to = 3*max(mu_matrix), by = 100)
        quantile_matrix <- matrix(nrow = 4, ncol = 7)
        
        # get CDFs
        for(h in 1:4){
          cdf_matrix <- matrix(NA, ncol = length(quantile_levels_nowcast), nrow = length(support))
          for(k in 1:ncol(cdf_matrix)){
            mu <- mu_matrix[h, k]
            sigma2 <- var_matrix[h, k]
            size <- pmin(abs(mu / (sigma2 / mu - 1)), 10000)
            cdf_matrix[, k] <- pnbinom(support, mu = mu, size = size)
          }
          # average CDFs
          cdf <- rowMeans(cdf_matrix)
          
          # get quantiles:
          for(k in seq_along(quantile_levels)){
            quantile_matrix[h, k] <- max(support[cdf <= quantile_levels[k]])
          }
          
          # get target end dates:
          t_e_d <- max(ts_temp$date) + 7*h
          n_quantile_levels <- length(quantile_levels)
          # store quantiles in data.frame:
          quantiles_formatted <- data.frame(location = rep("DE", n_quantile_levels),
                                            age_group = rep(ag, n_quantile_levels),
                                            forecast_date = rep(forecast_date, n_quantile_levels),
                                            target_end_date = rep(t_e_d, each = n_quantile_levels),
                                            horizon = rep(h, each = n_quantile_levels),
                                            type = "quantile",
                                            quantile = quantile_levels,
                                            value = quantile_matrix[h, ])
          # means:
          mean_formatted <- quantiles_formatted[1, ]
          mean_formatted$type <- "mean"
          mean_formatted$quantile <- NA
          mean_formatted$value <- mean(mu_matrix[h, ])
          
          # store / append:
          all_formatted <- rbind(quantiles_formatted, mean_formatted)
          
          if(is.null(fc)){
            fc <- all_formatted
          }else{
            fc <- rbind(fc, all_formatted)
          }
        }
      }
      
      
      # generate a plot if desired:
      if(plot_all){
        
        # truth data as of forecast_date, subset to relevant stratum
        reporting_triangle_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,
                                                      location = "DE", age_group = ag, max_lag = max_delay)
        
        
        # current version of truth data, subset to relevant stratum
        reporting_triangle_current <- data_as_of(dat_truth = triangles[[disease]], date = Sys.Date(),
                                                 location = "DE", age_group = ag, max_lag = max_delay)
        
        
        plot_data_back_in_time <- data.frame(date = as.Date(reporting_triangle_back_in_time$date),
                                             value = rowSums(reporting_triangle_back_in_time[, grepl("value_", colnames(reporting_triangle_back_in_time))],
                                                             na.rm = TRUE))
        plot_data_current <- data.frame(date = reporting_triangle_current$date,
                                        value = rowSums(reporting_triangle_current[, grepl("value_", colnames(reporting_triangle_current))],
                                                        na.rm = TRUE))
        
        
        plot_forecast(forecasts = fc,
                      location = "DE", age_group = ag,
                      truth = plot_data_back_in_time,
                      levels_coverage = c(0.5, 0.95),
                      start = as.Date("2023-08-31"),
                      # end = as.Date("2024-02-28"),
                      forecast_date = forecast_date,
                      # ylim = c(0, 2*max(tail(plot_data_back_in_time$value, 10)))
                      ylim = c(0,50000)
        )
        title(ag)
        lines(plot_data_current$date, plot_data_current$value, col = "red", lty  ="solid")
        title(paste0(disease, ",", loc, ", ", forecast_date))
      }
      
      # store in all_fc
      if(is.null(all_fc)){
        all_fc <- fc
      }else{
        all_fc <- rbind(all_fc, fc)
      }
      
      # file name:
      fl <- paste0(path_repo, "/submissions/", data_source, "/", disease, "/KIT-hhh4/",
                   forecast_date, "-", data_source, "-", disease, "-KIT-hhh4.csv")
      # get forecasts for national level (done separately)
      fc_pooled <- read.csv(fl, colClasses = c("forecast_date" = "Date",
                                               "target_end_date" = "Date"))
      fc_pooled <- subset(fc_pooled, age_group == "00+")
      # append:
      all_fc <- rbind(fc_pooled, fc)
      
      # write out:
      write.csv(all_fc, file = fl, row.names = FALSE)
    }
  }
}





