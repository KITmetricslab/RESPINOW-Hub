# compute the point forecast:
# Arguments:
#' @param observed: the observations / reporting triangle matrix
#' @param n_history: how many past observations to use to compute point forecast
#' @param remove observed: should available observations be removed in the return matrix?
#' @return a matrix of the asme dimensions as observed, but with the expectations added
compute_expectations <- function(observed, n_history = 60, remove_observed = TRUE){
  # restrict to last n_history observations
  observed <- tail(observed, n_history)
  nr <- nrow(observed)
  nc <- ncol(observed)
  # initialize results matrix:
  expectation <- observed
  # compute expectations iteratively
  for(co in 2:nc){
    block_top_left <- expectation[1:(nr - co + 1), 1:(co - 1), drop = FALSE]
    block_top <- expectation[1:(nr - co + 1), co, drop = FALSE]
    factor <- sum(block_top)/max(sum(block_top_left), 1)
    block_left <- expectation[(nr - co + 2):nr, 1:(co - 1), drop = FALSE]
    expectation[(nr - co + 2):nr, co] <- factor*rowSums(block_left)
  }
  # remove the observed values if desired:
  if(remove_observed){
    expectation[!is.na(observed)] <- NA
  }
  # avoid zero values:
  # expectation[expectation == 0] <- 0.2
  
  # return
  return(expectation)
}

#' extract numeric values of delay categories from column names of a reporting triangle
get_delays_numeric <- function(col_names){
  # handle minus one separately
  if(col_names[1] == "value_.1w"){
    col_names[1] <- "value-1w"
  }
  # handle ">" in last element separately, removing "." or ">"
  n <- length(col_names)
  col_names[n] <- gsub(".", "", col_names[n], fixed = TRUE)
  col_names[n] <- gsub(">", "", col_names[n], fixed = TRUE)
  
  # replace string bits:
  col_names <- gsub("value_", "", col_names)
  col_names <- gsub("w", "", col_names)
  
  # to numeric:
  num <- as.numeric(col_names)
  
  # handle last element separately, catch case where it was ">":
  if(num[length(num)] == num[length(num) - 1]){
    num[length(num)] <- num[length(num)] + 1
  }
  
  return(num)
}

# transform weekday from string to number
weekday_as_number <- function(weekday){
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if(!weekday %in% weekdays){
    stop("Argument weekday does not correspond to an English-language day of the week.")
  } else{
    which(weekdays == weekday)
  }
}

# get historic version of the data:
data_as_of <- function(dat_truth, age_group = "00+", location = "DE", date, weekday_data_updates = "Thursday", max_lag = NULL){
  # check arguments:
  if(!weekdays(date) == "Thursday") warning("date is usually expected to be a Thursday.")
  date <- as.Date(date)
  
  # subset to age group and location
  subs <- dat_truth[dat_truth$age_group == age_group &
                      dat_truth$location == location, ]
  # get matrix containing reporting triangle:
  matr <- subs[, grepl("value_", colnames(subs))]
  # find out which delays the columns correspond to:
  lags_numeric <- get_delays_numeric(col_names = colnames(matr))
  
  # restrict to columns up to max_lag if provided:
  if(!is.null(max_lag)){
    lags_numeric <- lags_numeric[lags_numeric <= max_lag]
    matr <- matr[, paste0("value_", lags_numeric, "w")]
  }
  
  # set up matrix with dates as in data (ends of weeks, typically Sundays)
  matr_dates <- matrix(subs$date, nrow = nrow(matr), ncol = ncol(matr))
  
  # set up matrix with delays corresponding to the columns:
  # how many days to add due to day of week when data are updated?
  weekday_data_updates_numeric <- weekday_as_number(weekday_data_updates)
  # delay matrix combining weeks (columns of triangle) and additional days (weekday_data_updates_numeric)
  matr_delays <- 7*matrix(lags_numeric, byrow = TRUE,
                          nrow = nrow(matr), ncol = ncol(matr)) + weekday_data_updates_numeric
  
  # matrix with dates when respective counts became available
  matr_reporting_date <- matr_dates + matr_delays
  matr[matr_reporting_date > date] <- NA
  
  # re-arrange into data.frame
  res <- data.frame(date = subs$date, matr)
  
  # remove rows with only NAs:
  inds_to_keep <- rowSums(!is.na(matr)) > 0
  res <- res[inds_to_keep, ]
  
  return(res)
}

# get the indices corresponding to the nowcasted quantities for a w-day rolling sum
#' @param observed the observations / reporting triangle matrix
#' @param d the nowcast horizon (d days back)
#' @param w the window size for the rolling sum, usually 7 days
#' @param n_history_expectations the number of past observations to use in the computations
indices_nowcast <- function(observed, d, w = 7, n_history_expectations = 60){
  observed <- tail(observed, n_history_expectations)
  res <- is.na(observed)
  res[1:(nrow(observed) - d - w), ] <- FALSE
  if(d > 0){
    res[(nrow(res) - d + 1):nrow(res), ] <- FALSE
  }
  return(res)
}

# fit the size parameter of a negative binomial via maximum likelihood
#' @param x the observed values
#' @param mu the expected values
fit_nb <- function(x, mu){
  nllik <- function(size){-sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)}
  opt <- optimize(nllik, c(0.1, 1000))
  opt$minimum
}

#' Generate a nowcast
#' @param observed the observations / reporting triangle data.frame
#' @param location the location for which to generate nowcasts
#' @param age_group the age group for which to generate nowcasts
#' @param min_horizon the minimum horizon for which to generate a nowcast (e.g., 2 for up to 2 days before the current date)
# 
# observed <- triangles[[disease]]
# location <- loc
# age_group <- "00+"
# forecast_date <- forecast_date
# n_history_expectations <- n_history_expectations
# n_history_dispersion <- n_history_dispersion
# min_horizon <- 0
# max_horizon <- max_horizon
# max_delay <- max_delay
# pathogen <- disease
# target_type <- "inc case"
# weekday_data_updates <- "Thursday"


compute_nowcast <- function(observed, location = "DE", age_group = "00+", forecast_date = NA, pathogen = NA,
                            min_horizon = 0, max_horizon = 4, weekday_end_of_week = "Sunday", weekday_data_updates = "Thursday",
                            max_delay = 4, n_history_expectations = 15, n_history_dispersion = 15){
  
  if(any(observed$date >= forecast_date)){
    message("Reporting triangle contains dates later than forecast_date. ", 
            # paste(observed$date[observed$date > forecast_date], collapse = ", "),
            " Note that data will be subsetted to those available on forecast_date (if applicable, negative delays are respected).")
  }
  
  if(weekdays(forecast_date) != weekday_data_updates){
    message("forecast_date is a different weekday than weekday_data_updates. This may be unintended.")
  }
  
  # which horizons need to be considered?
  horizons <- get_delays_numeric(col_names = colnames(observed)[grepl("value", colnames(observed))])
  horizons <- horizons[horizons <= max_horizon]
  n_horizons <- length(horizons)
  
  # bring to state as of forecast_date, subset to location and age group:
  observed_temp <- data_as_of(observed, age_group = age_group, location = location, date = forecast_date,
                              weekday_data_updates = weekday_data_updates, max_lag = max_delay)
  # turn into matrix:
  matr_observed <- as.matrix(observed_temp[, grepl("value", colnames(observed_temp))])
  # name rows:
  rownames(matr_observed) <- as.character(observed_temp$date)
  
  # generate point forecasts for current date and n_history_dispersion preceding weeks
  # these are necessary to estimate dispersion parameters
  # determine dates
  all_forecast_dates <- seq(from = forecast_date - 7*(n_history_dispersion), by = 7, to = forecast_date)
  # set up matrices to sore results:
  expectation_to_add <- # full expectations
    expectation_to_add_already_observed <- # expectations of the sum over already observable quantities
    to_add_already_observed <- # sums over the respective observed quantities
    matrix(NA, nrow = length(all_forecast_dates), ncol = ncol(matr_observed),
           dimnames = list(as.character(all_forecast_dates), NULL))
  
  for(t in seq_along(all_forecast_dates)){
    # identify date for which to compute retrospective nowcast
    forecast_date_temp <- all_forecast_dates[t]
    # bring to state of forecast_date_temp, subset to location and age group:
    observed_temp <- data_as_of(observed, age_group = age_group, location = location, date = forecast_date_temp,
                                weekday_data_updates = weekday_data_updates, max_lag = max_delay)
    # turn into matrix:
    matr_observed_temp <- as.matrix(observed_temp[, grepl("value", colnames(observed_temp))])
    # name rows:
    rownames(matr_observed_temp) <- as.character(observed_temp$date)
    
    # get same subset of the reporting triangle, but filled as far as possible at time of nowcast:
    matr_observed_temp_full <- matr_observed[which(rownames(matr_observed) %in% 
                                                     tail(rownames(matr_observed_temp), n_history_expectations)), ]
    # this is needed to estimate dispersion parameters below
    
    # generate retrospective point nowcast:
    point_forecasts_temp <- compute_expectations(matr_observed_temp, n_history = n_history_expectations)
    
    # structure by things already observed or not (necessary to use of partial observations)
    for(d in 1:n_horizons){
      # which indices in the matrix correspond to nowcasts at horizon d?
      inds_nowc <- indices_nowcast(matr_observed_temp, d = d - 1, w = 1,
                                   n_history_expectations = n_history_expectations)
      # compute sum of expected values for nowcast at horizon d over all elements of the reporting
      # triangle which have not yet been observed at time forecast_date_temp
      expectation_to_add[t, d] <- sum(point_forecasts_temp*inds_nowc, na.rm = TRUE)
      
      # which indices of the reporting triangle are already known at forecast_date?
      # (the one from the original function call, not forecast_date_temp)
      inds_already_observed <- !is.na(matr_observed_temp_full)
      
      # compute sum of expected values for nowcast at horizon d over all elements of the reporting
      # triangle which have not yet been observed at time forecast_date_temp, but have been observed at forecast_date
      expectation_to_add_already_observed[t, d] <- sum(point_forecasts_temp*inds_already_observed*inds_nowc, na.rm = TRUE)
      # compute the corresponding observed values
      to_add_already_observed[t, d] <- sum(matr_observed_temp_full*
                                             inds_already_observed*inds_nowc, na.rm = TRUE)
    }
  }
  
  # have to remove last row to estimate dispersion
  expectation_to_add_already_observed <- expectation_to_add_already_observed[-nrow(expectation_to_add_already_observed), ]
  to_add_already_observed <- to_add_already_observed[-nrow(to_add_already_observed), ]
  
  # estimate dispersion
  size_params <- numeric(n_horizons)
  to_keep <- expectation_to_add_already_observed[, 1] > 0
  to_keep[length(to_keep)] <- FALSE
  to_add_already_observed <- to_add_already_observed[to_keep, ]
  expectation_to_add_already_observed <- expectation_to_add_already_observed[to_keep, ]
  
  # run through horizons
  for(i in 1:n_horizons){
    obs_temp <- to_add_already_observed[, i]
    mu_temp <- expectation_to_add_already_observed[, i] + 0.1 # plus 0.1 to avoid ill-defined NB
    size_params[i] <- fit_nb(x = obs_temp, 
                             mu = mu_temp)
  }
  
  
  # generate actual nowcast in standard format:
  mu <- expectation_to_add[nrow(expectation_to_add), ]
  quantile_levels <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  df_all <- NULL
  
  # run through horizons:
  for(d in 1:n_horizons){
    # get numeric horizon - only needed in creation of data.frame
    h <- horizons[d]
    
    # by how much do we need to shift quantiles upwards? Note that this needs to use index d
    already_observed <- sum(matr_observed_temp[nrow(matr_observed_temp) - d + 1, ], na.rm = TRUE)
    
    # data frame for expecations:
    weekday_data_updates_numeric <- weekday_as_number(weekday_data_updates)
    df_mean <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - weekday_data_updates_numeric - 7*h,
                          horizon = -h,
                          type = "mean",
                          quantile = NA,
                          value = round(mu[d] + already_observed),
                          pathogen = pathogen)
    
    # obtain quantiles:
    qtls0 <- qnbinom(quantile_levels, 
                     size = size_params[d], mu = mu[d])
    # shift them up by already oberved values
    qtls <- qtls0 + already_observed
    # data.frame for quantiles:
    df_qtls <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - weekday_data_updates_numeric - 7*h,
                          horizon = -h,
                          type = "quantile",
                          quantile = quantile_levels,
                          value = qtls,
                          pathogen = pathogen)
    
    # join:
    df <- rbind(df_mean, df_qtls)
    
    # add to results from other horizons
    if(is.null(df_all)){
      df_all <- df
    }else{
      df_all <- rbind(df_all, df)
    }
  }
  
  # return
  return(list(result = df_all,
              mu = mu, size_params = size_params,
              expectation_to_add_already_observed = expectation_to_add_already_observed,
              to_add_already_observed = to_add_already_observed))
}


######## Plotting functions:

# get the subset of a forecast file needed for plotting:
subset_forecasts_for_plot <- function(forecasts, forecast_date = NULL, horizon, location, age_group, type = NULL){
  # check_target <- if(is.null(horizon)){
  #   grepl(target_type, forecasts$target)
  # } else{
  #   grepl(horizon, forecasts$target) & grepl(target_type, forecasts$target)
  # }
  check_forecast_date <- if(is.null(forecast_date)) TRUE else forecasts$forecast_date == forecast_date
  
  forecasts <- forecasts[ # check_target &
                           check_forecast_date &
                           forecasts$location == location &
                           forecasts$age_group == age_group, ]
  if(!is.null(type)) forecasts <- forecasts[forecasts$type == type, ]
  return(forecasts)
}

determine_ylim <- function(forecasts, forecast_date = NULL, horizon, location, age_group, truth, start_at_zero = TRUE){
  truth <- subset(truth, date >= forecast_date - 28)
  forecasts <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                         horizon = horizon, location = location, age_group = age_group)
  lower <- if(start_at_zero){
    0
  }else{
    0.95*min(c(forecasts$value, truth$value))
  }
  if("location" %in% colnames(truth)){
    truth <- truth[truth$location == location, ]
  }
  ylim <- c(lower, 1.05* max(c(forecasts$value, truth$value)))
}

# create an empty plot to which forecasts can be added:
empty_plot <- function(xlim, ylim, xlab, ylab){
  plot(NULL, xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = "", axes = FALSE)
  axis(2, las = 1)
  title(ylab = ylab, line = 4)
  all_dates <- seq(from = as.Date("2020-02-01"), to = Sys.Date() + 28, by  =1)
  saturdays <- all_dates[weekdays(all_dates) == "Saturday"]
  axis(1, at = saturdays, labels = as.Date(saturdays, origin = "1970-01-01"))
  box()
}

# add a single prediction interval:
draw_prediction_band <- function(forecasts, forecast_date = NULL, horizon,
                                 location, age_group, coverage, col = "lightgrey"){
  if(!coverage %in% c(1:9/10, 0.95, 0.98)) stop("Coverage needs to be from 0.1, 0.2, ..., 0.9, 0.95, 0.98")
  
  forecasts <- subset_forecasts_for_plot(forecasts  =forecasts, forecast_date = forecast_date,
                                         horizon = horizon, location = location,
                                         age_group = age_group, type = "quantile")
  
  # select points to draw polygon:
  lower <- subset(forecasts, abs(quantile - (1 - coverage)/2) < 0.01)
  lower <- lower[order(lower$target_end_date), ]
  upper <- subset(forecasts, abs(quantile - (1 - (1 - coverage)/2)) < 0.01)
  upper <- upper[order(upper$target_end_date, decreasing = TRUE), ]
  # draw:
  polygon(x = c(lower$target_end_date, upper$target_end_date),
          y = c(lower$value, upper$value), col = col, border = NA)
}

# draw many prediction intervals (resulting in a fanplot)
draw_fanplot <- function(forecasts, forecast_date, horizon, location, age_group, levels_coverage = c(1:9/10, 0.95, 0.98),
                         cols = colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]){
  for(i in rev(seq_along(levels_coverage))){
    draw_prediction_band(forecasts = forecasts,
                         horizon = horizon,
                         forecast_date = forecast_date,
                         location = location,
                         age_group = age_group,
                         coverage = levels_coverage[i],
                         col = cols[i])
  }
}

# add points for point forecasts:
draw_points <- function(forecasts, horizon, forecast_date, location, age_group, col = "deepskyblue4"){
  forecasts <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                         horizon = horizon, location = location,
                                         age_group = age_group, type = "quantile")
  points <- subset(forecasts, quantile == 0.5)
  # if no medians available: use means:
  if(nrow(points) == 0){
    points <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                        horizon = horizon, location = location,
                                        age_group = age_group, type = "mean")
  }
  lines(points$target_end_date, points$value, col = col, lwd = 2)
}

# add smaller points for truths:
draw_truths <- function(truth, location){
  lines(truth$date, truth$value, pch = 20, lwd = 2)
}

# wrap it all up into one plotting function:
# Arguments:
# forecasts a data.frame containing forecasts from one model in he standard long format
# needs to contain forecasts from different forecast_dates to plot forecats by "horizon"
# horizon: "1 wk ahead", "2 wk ahead", "3 wk ahead" or "4 wk ahead"; if specified forecasts at this horizon
# are plotted for different forecast dates. Has to be NULL if forecast_date is specified
# forecast_date: the date at which forecasts were issued; if specified, 1 though 4 wk ahead forecasts are shown
# Has to be NULL if horizon is specified
# location: the location for which to plot forecasts
# truth: a truth data set in the standard format; has to be chosen to match target_type
# levels_coverage: which intervals are to be shown? Defaults to all, c(0.5, 0.95) is a reasonable
# parsimonious choice.
# start, end: beginning and end of the time period to plot
# ylim: the y limits of the plot. If NULL chosen automatically.
# cols: a vector of colors of the same length as levels_coverage
plot_forecast <- function(forecasts,
                          horizon = NULL,
                          forecast_date = NULL,
                          location,
                          age_group,
                          truth,
                          levels_coverage = c(1:9/10, 0.95, 0.98),
                          start = as.Date("2020-04-01"),
                          end = Sys.Date() + 28,
                          ylim = NULL,
                          cols_intervals = colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1],
                          col_point = "deepskyblue4",
                          xlab = "date",
                          ylab = "inc",
                          start_at_zero = TRUE){
  
  if(is.null(horizon) & is.null(forecast_date)) stop("Exactly one out of horizon and forecast_date needs to be specified")
  
  forecasts <- subset(forecasts, target_end_date >= start - 7 & target_end_date <= end)
  forecasts <- forecasts[order(forecasts$target_end_date), ]
  # truth <- truth[truth$date >= start & truth$location == location, ]
  xlim <- c(start, end)
  
  if(is.null(ylim)) ylim <- determine_ylim(forecasts = forecasts, forecast_date = forecast_date,
                                           horizon = horizon,
                                           location = location, age_group = age_group,
                                           truth = truth, start_at_zero = start_at_zero)
  
  empty_plot(xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  if(!is.null(forecast_date)) abline(v = forecast_date, lty = "dashed")
  draw_fanplot(forecasts = forecasts, 
               horizon = horizon, forecast_date = forecast_date,
               location = location, age_group = age_group, levels_coverage = levels_coverage,
               cols = cols_intervals)
  draw_points(forecasts = forecasts,
              horizon = horizon, forecast_date = forecast_date,
              location = location, age_group = age_group, col = col_point)
  draw_truths(truth = truth, location = location)
}