library(dplyr)
library(magrittr)
library(stats)
library(tidyr)
library(lubridate)
library(data.table)
library(cmdstanr)
library(ggplot2)
library(epinowcast)

# set working directory
current_path = rstudioapi::getActiveDocumentContext()$path # get path of this file
setwd(dirname(current_path))

data_source <- "survstat"
pathogen <- "influenza"

# source functions:
source("functions_epinowcast.R")

show_plot <- TRUE
forecast_dates <- seq(from = as.Date("2024-01-11"),
                      to = as.Date("2024-01-11"),
                      by = 7)

# get data:
triangle <- read.csv(paste0("../../data/", data_source, "/", pathogen, "/reporting_triangle-",
                            data_source, "-", pathogen, "-preprocessed.csv"))
triangle[triangle == 0] <- 1 # needed to avoid error message "Empirical max delay is less than the specified max delay."

# if the reporting triangle contains a -1 week delay column we need to shift things in some places...
contains_minus_one_lags <- any(c("value_.1w", "value_-1w") %in%  colnames(triangle))

# transform data to epinowcast format:
triangle <- convert_reptri_to_long(triangle, unit = 7)# unit = 7 for weekly data



for(i in seq_along(forecast_dates)){
  forecast_date <- forecast_dates[i] #  as.Date("2023-01-08")
  cat(paste(forecast_date, "\n"))
  
  # restrict to relevant parts:
  debug(restrict_reptri)
  subs <- restrict_reptri(data = triangle, forecast_date = forecast_date + 7*contains_minus_one_lags,
                          moving_window = 30, max_d = 4 + contains_minus_one_lags, 
                          locations = "DE", age_groups = "00+") #, age_groups = "00+")
  
  if(show_plot){
    subs_latest <- restrict_reptri(data = triangle, forecast_date = max(triangle$reference_date),
                                   moving_window = nrow(triangle), max_d = 4, 
                                   locations = "DE", age_groups = "00+")
    subs_latest <- subset(subs_latest, reference_date %in% subs$reference_date)
  }
  
  
  # transform to a daily format epinowcast will be able to use:
  earliest_date <- min(subs$reference_date) # identify first date
  subs <- pretend_daily_reptri(subs, max_d = 4, earliest_date = earliest_date)
  
  # set reference time indexed reporting process model
  ref_f <- ~1 # + (1|.group)
  reference_module <- enw_reference(ref_f, distribution = "lognormal", data = subs)
  
  # set reporting time indexed reporting process model
  rep_f <- ~1
  report_module <- enw_report(rep_f , data = subs)
  
  # set expectation module:
  exp_f <- ~rw(day, by = .group)
  expectation_module <- enw_expectation(exp_f, data = subs)
  
  # compile STAN model
  model <- enw_model(threads = TRUE)
  
  # set number of cores
  mc.cores <- 2
  options(mc.cores = mc.cores)
  
  # run
  nowcast <- epinowcast(subs,
                        reference = reference_module,
                        report = report_module,
                        expectation = expectation_module,
                        fit = enw_fit_opts(save_warmup = FALSE, pp = TRUE,
                                           chains = 2, threads_per_chain = 2,
                                           iter_sampling = 500, iter_warmup = 500,
                                           show_messages = FALSE, refresh = 0 
                        ),
                        model = model
  )
  
  if(show_plot){
    earliest_date_latest <- min(subs_latest$reference_date) # identify first date
    subs_latest2 <- pretend_daily_reptri(subs_latest, max_d = 4, earliest_date = earliest_date_latest)
    
    plot(nowcast, latest_obs = tail(subs_latest2$latest[[1]], 8)) + facet_wrap(vars(age_group), scales = "free")
  }
  
  result <- reformat_nowcast_to_weekly(nowcast, 
                                       forecast_date = forecast_date,
                                       earliest_date = earliest_date,
                                       pathogen = pathogen)
  result$pathogen <- NULL
  
  dir <- paste0("../../submissions/", data_source, "/", pathogen, "/KIT-epinowcast")
  dir.create(dir)
  file <- paste0(dir, "/", forecast_date, "-", data_source, "-", pathogen, "-KIT-epinowcast.csv")
  write.csv(result, file = file,
            row.names = FALSE)
  
}















