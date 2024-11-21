#' Pre-process reporting triangle
#' An R implementation of the pre-processing done in the repo (there in Python)
#' Arguments:
#' @param df the reporting triangle as a data.frame
preprocess_reporting_triangle <- function(df, max_delay = 4) {
  
  # restrict to first 5 columns.
  col_4w <- which(colnames(df) == paste0("value_", max_delay, "w"))
  df <- df[, 1:col_4w]
  
  # columns containing values:
  value_cols <- which(grepl("value", names(df)))
  
  # Loop over each row
  for (i in 1:nrow(df)) {
    to_subtract <- 0
    row <- df[i, ]
    
    # Loop over the columns starting from the last column back to column 5
    for (j in rev(value_cols)) {
      value <- row[[j]]
      
      if (!is.na(value)) {
        value <- value + to_subtract
        
        if (value < 0) {
          to_subtract <- value
          df[i, j] <- 0
        } else {
          df[i, j] <- value
          to_subtract <- 0
        }
      }
    }
  }
  
  # Convert 'value' columns to integer type
  for (col in value_cols) {
    df[[col]] <- as.integer(df[[col]])
  }
  
  return(df)
}

# Compute the point forecast:
# Arguments:
#' @param observed: the observations / reporting triangle matrix for which to produce nowcasts
#' @param observed2: the observations / reporting triangle matrix pooled across strata.
#' This can be used to estimate delay distributions more reliably if they are similar across strata.
#' Only used if borrow_delays == TRUE.
#' @param borrow_delays: should the delay distribution be estimated from a separate data set observed2?
#' @param n_history: how many past observations to use to compute point forecast
#' @param remove_observed: should available observations be removed in the return matrix?
#' @return a matrix normally of the same dimensions as observed (possibly padded), but with the expectations added
compute_expectations <- function(observed, observed2 = NULL, borrow_delays = FALSE, n_history = 60, remove_observed = TRUE){
  
  # # turn into matrices:
  # dates <- as.character(observed$date)
  # observed <- as.matrix(observed[, grepl("value", colnames(observed))])
  # rownames(observed) <- dates
  
  if(borrow_delays){
    # catch missing observed2
    if(is.null(observed2)) stop("observed2 is needed if borrow_delays == TRUE")
  }else{
    # setting observed2 to be observed (i.e., delays are estimated from observed)
    # if delay estimation is not to be shared.
    # having a variable observed2 in any case makes the code simpler.
    observed2 <- observed
    if(!is.null(observed2) & !identical(observed, observed2)){
      warning("observed2 is not used as borrow_delays == FALSE")
    } 
  }
  
  # catch too short history: observed2 needs n_history observations
  if(nrow(observed2) < n_history){
    stop("Available reporting triangles are too short for the chosen n_history.")
  }
  # observed can have fewer and may be padded to the right size
  observed <- pad_matr(observed, n_history)
  
  # restrict to last n_history observations
  observed <- tail(observed, n_history)
  observed2 <- tail(observed2, n_history)
  nr <- nrow(observed)
  nc <- ncol(observed)
  
  # initialize results matrix:
  expectation <- observed
  # compute expectations iteratively
  for(co in 2:nc){
    # using observed2 to compute multiplication factors:
    block_top_left <- observed2[1:(nr - co + 1), 1:(co - 1), drop = FALSE]
    block_top <- observed2[1:(nr - co + 1), co, drop = FALSE]
    factor <- sum(block_top)/max(sum(block_top_left), 1)
    # but of course apply to expectation
    block_left <- expectation[(nr - co + 2):nr, 1:(co - 1), drop = FALSE]
    expectation[(nr - co + 2):nr, co] <- factor*rowSums(block_left)
  }
  # remove the observed values if desired:
  if(remove_observed){
    expectation[!is.na(observed)] <- NA
  }
  
  # return
  return(expectation)
}

#' Pad a reporting triangle which is too short.
#' @param matr_observed the reporting triangle
#' @param n_history the minimum number of rows needed
pad_matr <- function(matr_observed, n_history){
  if(nrow(matr_observed) < n_history){
    # block of NAs to add
    to_add <- matrix(NA, ncol = ncol(matr_observed),
                     nrow = n_history - nrow(matr_observed))
    colnames(to_add) <- colnames(matr_observed)
    # append
    matr_observed <- rbind(to_add, matr_observed)
    # name rows correctly again
    to <- as.Date(tail(rownames(matr_observed), 1))
    rn <- as.character(seq(to = to, 
                           from = to - 7*(nrow(matr_observed) - 1),
                           by = 7))
    rownames(matr_observed) <- rn
  }
  return(matr_observed)
}

#' Fit dispersion parameters:
#' - either size of a negative binomial 
#' - or sd of a normal
#' this is based on historical point nowcasts and observations
#' See compute_nowcast for documentation of arguments.
fit_dispersion <- function(observed, location, age_group,
                           observed2 = NULL, location2 = NULL, age_group2 = NULL,
                           type = "size", forecast_date, max_delay,
                           borrow_delays = FALSE, borrow_dispersion = FALSE,
                           n_history_expectations, n_history_dispersion,
                           weekday_data_updates = "Thursday"){
  
  if(borrow_delays){
    # catch missing observed2
    if(is.null(observed2)) stop("observed2 is needed if borrow_delays == TRUE")
  }else{
    if(!is.null(observed2)) warning("observed2 is not used as borrow_delays == FALSE")
    observed2 <- observed
  }
  
  if(!type %in% c("size", "sd")){
    stop("type needs to be either size or sd.")
  }
  
  # if no observed2 is provided: use observed
  # having an object observed2 in any case makes the code simpler in the following
  if(is.null(observed2)){
    observed2 <- observed
  }
  
  
  # bring reporting triangles to state as of forecast date:
  matr_observed <- data_as_of(observed, age_group = age_group, location = location, date = forecast_date,
                              weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
  # pad matr_observed with NAs if too short:
  matr_observed <- pad_matr(matr_observed, n_history_expectations + n_history_dispersion)
  matr_observed2 <- data_as_of(observed2, age_group = age_group2, location = location2, date = forecast_date,
                               weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
  # no padding here as matr_observed2 cannot be too short.
  
  # check length of available reporting triangles.
  if(nrow(observed2) < n_history_expectations + n_history_dispersion){
    stop("Available reporting triangles are too short for the chosen n_history_expectations and n_history_dispersion.")
  }
  if(!borrow_delays & nrow(observed) < n_history_expectations){
    stop("Available reporting triangles are too short for the chosen n_history_expectations and n_history_dispersion.")
  }
  
  # generate point forecasts for n_history_dispersion preceding weeks
  # these are necessary to estimate dispersion parameters
  # determine dates
  if(type == "size"){
    # for size parameters there is a smart way of using partial observations
    all_forecast_dates <- seq(from = forecast_date - 7*(n_history_dispersion), by = 7,
                              to = forecast_date - 7) # exclude actual forecast date
  }else{
    # for sds incomplete observations need to be excluded
    all_forecast_dates <- seq(from = forecast_date - 7*(n_history_dispersion), by = 7,
                              to = forecast_date - (max_delay - 1)*7)
  }
  
  
  # set up matrices to store results.
  # each of these contains the forecast dates in the rows and horizons in the columns
  # (i.e., columns are not delays, but horizons, and each cell contains a total
  # value corresponding to that horizon, e.g., the total expected value to add)
  n_horizons <- ncol(matr_observed) - 1
  expectation_to_add <- # full expectations of counts to add
    expectation_to_add_already_observed <- # expectations of the sum over already observable quantities
    to_add_already_observed <- # sums over the respective observed quantities
    matrix(NA, nrow = length(all_forecast_dates), ncol = n_horizons,
           dimnames = list(as.character(all_forecast_dates), NULL))
  
  # run through forecast dates to generate point nowcasts and corresponding observations:
  for(t in seq_along(all_forecast_dates)){
    # identify date for which to compute retrospective nowcast
    forecast_date_temp <- all_forecast_dates[t]
    # bring to state of forecast_date_temp, subset to location and age group:
    matr_observed_temp <- data_as_of(observed, age_group = age_group, location = location, date = forecast_date_temp,
                                     weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
    matr_observed2_temp <- data_as_of(observed2, age_group = age_group2, location = location2, date = forecast_date_temp,
                                      weekday_data_updates = weekday_data_updates, max_lag = max_delay, return_matrix = TRUE)
    
    # pad and catch case where matr_observed does not contain any data
    if(nrow(matr_observed_temp) > 0){
      matr_observed_temp <- pad_matr(matr_observed_temp, n_history_expectations)
    }else{
      matr_observed_temp <- NA*matr_observed2_temp
    }
    
    # get same subset of the reporting triangle, but filled as far as possible at forecast_date:
    matr_observed_temp_full <- matr_observed[which(rownames(matr_observed) %in% 
                                                     tail(rownames(matr_observed_temp), 
                                                          n_history_expectations)), ]
    # this is needed to estimate dispersion parameters below
    
    # generate retrospective point nowcast:
    point_forecasts_temp <- compute_expectations(observed = matr_observed_temp,
                                                 observed2 = matr_observed2_temp,
                                                 n_history = n_history_expectations,
                                                 borrow_delays = borrow_delays, 
                                                 remove_observed = TRUE)
    
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
      to_add_already_observed[t, d] <- sum(matr_observed_temp_full*inds_already_observed*inds_nowc, na.rm = TRUE)
    }
  }
  
  # estimate dispersion
  disp_params <- numeric(ncol(expectation_to_add))
  # remove rows with zero initial reports (Christmas etc)
  to_keep <- abs(expectation_to_add_already_observed[, 1]) >= 0.1
  to_add_already_observed <- to_add_already_observed[to_keep, ]
  expectation_to_add_already_observed <- expectation_to_add_already_observed[to_keep, ]
  
  if(nrow(to_add_already_observed) == 0){
    stop("Cannot estimate nowcast dispersion from available data. Maybe try borrow_dispersion = TRUE.")
  }
  
  # run through horizons
  for(i in 1:n_horizons){
    obs_temp <- to_add_already_observed[, i]
    mu_temp <- expectation_to_add_already_observed[, i]
    if(type == "size"){
      mu_temp <- mu_temp + 0.1
      # plus 0.1 to avoid ill-defined negative binomial
      disp_params[i] <- fit_nb(x = obs_temp, mu = mu_temp)
    }else{
      disp_params[i] <- sd(obs_temp - mu_temp)
    }
  }
  
  return(disp_params)
}


#' extract numeric values of delay categories from column names of a reporting triangle
#' @param col_names a vector of column names. Delay columns must have format "value_<integer>w".
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

#' transform weekday from string to number
#' @param weekday: the weekday as a string (in English)
weekday_as_number <- function(weekday){
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if(!weekday %in% weekdays){
    stop("Argument weekday does not correspond to an English-language day of the week.")
  } else{
    which(weekdays == weekday)
  }
}

#' Get historic version of the reporting triangle as of a given date, for a given
#' age group and location.
#' @param dat_truth the reporting triangle in the data.frame format.
#' @param age_group: the age group for which to get the reporting triangle.
#' @param location the location
#' @param date the date
#' @param weekday_data_updates weekday on which updates occur. Necessary to know
#' which updates have already been made at a given point in time.
#' @param return_matrix should the reporting triangle be returned as a matrix rather
#' than a data.frame?
data_as_of <- function(dat_truth, age_group = "00+", location = "DE", date, 
                       weekday_data_updates = "Thursday", max_lag = NULL,
                       return_matrix = FALSE){
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
  
  # remove rows with only NAs:
  inds_to_keep <- rowSums(!is.na(matr)) > 0
  matr <- matr[inds_to_keep, ]
  
  if(return_matrix){
    rownames(matr) <- subs$date[inds_to_keep]
    return(matr)
  }else{
    return(data.frame(subs[inds_to_keep,
                           c("location", "age_group", "year", "week", "date")],
                      matr))
  }
}

# get the indices corresponding to the nowcasted quantities for a w-day rolling sum
#' @param observed the observations / reporting triangle matrix
#' @param d the nowcast horizon (d days back)
#' @param w the window size for the rolling sum (typically only needed for daily data)
#' @param n_history_expectations the number of past observations to use in the computations
indices_nowcast <- function(observed, d, w = 1, n_history_expectations){
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
  if(length(x) == 0) return(NA)
  nllik <- function(size){-sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)}
  opt <- optimize(nllik, c(0.1, 1000))
  opt$minimum
}

#' Generate a nowcast
#' @param observed the observations / reporting triangle data.frame
#' @param location the location for which to generate nowcasts
#' @param age_group the age group for which to generate nowcasts
#' @param forecast_date the date when the nowcast is issued. The function automatically restricts
#' the reportng triangle if an earlier forecast_date is provided to imitate real-time nowcasting.
#' @param borrow_delays if borrow_delays == TRUE, a separate reporting triangle observed2 is used to
#' estimate the delay distribution. Typically used to share strength across strata (then observed2,
#' location2, age_group2 coresponds to a pooled reporting triangle).
#' @param borrow_dispersion if borrow_delays == TRUE, the separate reporting triangle is also used to
#' estimate the nowcast dispersion.
#' @param observed2: the observations / reporting triangle matrix pooled across strata.
#' This can be used to estimate delay distributions more reliably if they are similar across strata.
#' @param location2 the location used to estimate the delay distribution (and possibly dispersion).
#' @param age_group2 the age group used to estimate the delay distribution (and possibly dispersion).
#' @param weekday_end_of_week the weekday when data updates happen (needed to know which data were available when).
#' @param max_delay the maximum delay considered.
#' @param n_history_expectations the number of observations used to estimate the delay distribution
#' @param n_history_dispersion the number of re-computed nowcasts used to estimate the error variance
#' @param quantile_levels the predictive quantile levels to return
compute_nowcast <- function(observed, location = "DE", age_group = "00+",
                            forecast_date, type = "additions",
                            borrow_delays = FALSE, borrow_dispersion = FALSE,
                            observed2 = NULL, location2 = NULL, age_group2 = NULL,
                            weekday_data_updates = "Thursday",
                            max_delay = 4, n_history_expectations = 15, n_history_dispersion = 15, 
                            quantile_levels = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){
  
  # check reporting triangle and forecast_date:
  if(forecast_date > max(observed$date) + 7){
    stop("Reporting triangle not up to date. forecast_date can be at most max(observed$date) + 7.")
  }
  
  if(any(observed$date >= forecast_date)){
    message("Reporting triangle contains dates later than forecast_date. ", 
            " Note that data will be subsetted to those available on forecast_date (if applicable, negative delays are respected).")
  }
  
  if(weekdays(forecast_date) != weekday_data_updates){
    message("forecast_date is a different weekday than weekday_data_updates. This may be unintended.")
  }
  
  # Can only use observed2 for dispersion if also used for delay distribution:
  if(borrow_dispersion & !borrow_delays){
    stop("borrow_dispersion == TRUE is only allowed of borrow_delays == TRUE")
  }
  
  # Check type is allowed
  if(!type %in% c("additions", "revise_average")){
    stop("type needs to be either 'additions' or 'revise_average'.")
  }
  
  # pre-process reporting triangle.
  # note: only done for type == "additions" where nowcasting methods assume positive increments.
  if(type == "additions"){
    observed <- preprocess_reporting_triangle(observed)
    if(!is.null(observed2)){
      observed2 <- preprocess_reporting_triangle(observed2)
    }
  }
  
  # which horizons need to be considered?
  horizons <- get_delays_numeric(col_names = colnames(observed)[grepl("value", colnames(observed))])
  max_horizon <- max_delay - 1 # no need to predict for maximum horizon
  horizons <- horizons[horizons <= max_horizon]
  n_horizons <- length(horizons)
  
  # bring to state as of forecast_date, subset to location and age group:
  observed_as_of <- data_as_of(observed, age_group = age_group, location = location,
                               date = forecast_date, weekday_data_updates = weekday_data_updates,
                               max_lag = max_delay, return_matrix = TRUE)
  # pad with NAs if needed
  observed_as_of <- pad_matr(observed_as_of, n_history_expectations + n_history_dispersion)
  # if sharing desired: same for observed2_as_of
  if(borrow_delays){
    observed2_as_of <- data_as_of(observed2, age_group = age_group2, location = location2, 
                                  date = forecast_date, weekday_data_updates = weekday_data_updates, 
                                  max_lag = max_delay, return_matrix = TRUE)
    # no padding here as observed2_as_of needs to have the desired size
  }else{ # if no sharing desired: set all _2 variables to their regular counterparts
    if(!is.null(observed2) | !is.null(location2) | !is.null(age_group2)){
      warning("observed2, location2 or age_group2 were provided despite borrow_delays == FALSE. These will be ignored.")
    }
    observed2_as_of <- observed_as_of
    location2 <- location
    age_group2 <- age_group
  }
  
  # generate point nowcast:
  point_forecast <- compute_expectations(observed = observed_as_of, 
                                         observed2 = observed2_as_of,
                                         n_history = n_history_expectations, 
                                         borrow_delays = borrow_delays)
  # estimate size parameters for negative binomial:
  disp_params <- fit_dispersion(observed = if(borrow_dispersion) observed2 else observed,
                           location = if(borrow_dispersion) location2 else location,
                           age_group = if(borrow_dispersion) age_group2 else age_group,
                           observed2 = observed2,
                           location2 = location2,
                           age_group2 = age_group2,
                           type = switch(type, "additions" = "size",
                                         "revise_average" = "sd"),
                           forecast_date = forecast_date,
                           max_delay = max_delay,
                           borrow_delays = borrow_delays,
                           borrow_dispersion = borrow_dispersion,
                           n_history_expectations = n_history_expectations,
                           n_history_dispersion = n_history_dispersion,
                           weekday_data_updates = weekday_data_updates)
  
  # bring actual nowcast into standard format:
  mu <- rev(rowSums(point_forecast, na.rm = TRUE))[1:n_horizons] # re-order expecations
  # set up data frame to store:
  df_all <- NULL
  
  # run through horizons:
  for(d in 1:n_horizons){
    # get numeric horizon - only needed in creation of data.frame
    h <- horizons[d]
    
    # by how much do we need to shift quantiles upwards? Note that this needs to use index d
    already_observed <- sum(observed_as_of[nrow(observed_as_of) - d + 1, ], na.rm = TRUE)
    
    # data frame for expecations:
    weekday_data_updates_numeric <- weekday_as_number(weekday_data_updates)
    df_mean <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - weekday_data_updates_numeric - 7*h,
                          horizon = -h,
                          type = "mean",
                          quantile = NA,
                          value = round(mu[d] + already_observed))
    
    # obtain quantiles:
    if(type == "additions"){
      qtls0 <- qnbinom(quantile_levels, 
                       size = disp_params[d], mu = mu[d])
    }else{
      qtls0 <- qnorm(quantile_levels, 
                       sd = disp_params[d], mean = mu[d])
    }
    
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
                          value = qtls)
    
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
              mu = mu, size_params = disp_params))# ,
  # expectation_to_add_already_observed = expectation_to_add_already_observed,
  # to_add_already_observed = to_add_already_observed))
}


######## Plotting functions:

# get the subset of a forecast file needed for plotting:
subset_forecasts_for_plot <- function(forecasts, forecast_date = NULL, horizon, location, age_group, type = NULL){
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
