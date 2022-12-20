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

# restrict a reporting triangle to the information available at a given time point t
#' @param observed the observations / reporting triangle matrix
#' @param t said time point
back_in_time <- function(observed, t){
  observed <- observed[1:t, ]
  for(i in 1:(ncol(observed) - 1)){
    observed[t - i + 1, (i + 1):ncol(observed)] <- NA
  }
  observed
}

# wrapper around back_in_time to apply it to a data frame in our usual format
#' @param observed the observations / reporting triangle data.frame
#' @param date date (which data version should be retrieved?)
back_in_time_df <- function(observed, date){
  observed <- observed[observed$date <= date, ]
  cols_value <- grepl("value_", colnames(observed))
  matr <- as.matrix(observed[, cols_value])
  for(i in 0:(ncol(matr) - 2)){
    matr[nrow(matr) - i, (i + 2):ncol(matr)] <- NA
  }
  observed[, cols_value] <- matr
  return(observed)
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
compute_nowcast <- function(observed, location = "DE", age_group = "00+", pathogen = NA, target_type = "",
                            min_horizon = 2, max_horizon = 28, 
                            max_delay = 40, n_history_expectations = 60, n_history_dispersion = 60){
  
  # subset to location and age group:
  observed <- subset(observed, location == location & age_group == age_group)
  
  # reporting triangle as matrix
  matr_observed <- as.matrix(observed[, grepl("value", colnames(observed))])
  # reduce to max delay:
  matr_observed <- cbind(matr_observed[, 1:(max_delay + 1)], 
                         matrix(rowSums(matr_observed[, -(1:(max_delay))], 
                                        na.rm = TRUE), ncol = 1))
  
  colnames(matr_observed)[max_delay + 2] <- paste0("value_", max_delay + 1, "w")
  observed[nrow(matr_observed) - 0:max_delay, max_delay + 2] <- NA
  rownames(matr_observed) <- observed$date
  
  nr <- nrow(matr_observed)
  nc <- ncol(matr_observed)
  
  # compute point forecasts
  expectation_to_add <- # full expectations
    expectation_to_add_already_observed <- # expectations of the sum over already observable quantities
    to_add_already_observed <- # sums over the respective observed quantities
    matrix(NA, nrow = nr, ncol = max_horizon + 1,
           dimnames = list(observed$date, NULL))
  
  # generate point forecasts for current date and n_history_dispersion preceding weeks
  # these are necessary to estimate dispersion parameters
  for(t in (nr - n_history_dispersion):nr){
    matr_observed_temp <- back_in_time(matr_observed, t)
    point_forecasts_temp <- compute_expectations(matr_observed_temp, n_history = n_history_expectations)
    
    for(d in min_horizon:max_horizon){
      inds_nowc <- indices_nowcast(matr_observed_temp, d = d, n_history_expectations = n_history_expectations)
      inds_already_observed <- tail(!is.na(matr_observed[1:t, ]), n_history_expectations)
      
      expectation_to_add[t, d + 1] <- sum(point_forecasts_temp*inds_nowc, na.rm = TRUE)
      expectation_to_add_already_observed[t, d + 1] <- sum(point_forecasts_temp*inds_already_observed*inds_nowc, na.rm = TRUE)
      to_add_already_observed[t, d + 1] <- sum(tail(matr_observed[1:t, ], n_history_expectations)*
                                                 inds_already_observed*inds_nowc, na.rm = TRUE)
    }
  }
  
  # remove last row to estimate dispersion
  expectation_to_add_already_observed <- expectation_to_add_already_observed[-nrow(expectation_to_add_already_observed), ]
  to_add_already_observed <- to_add_already_observed[-nrow(to_add_already_observed), ]
  
  # estimate dispersion
  size_params <- numeric(max_horizon +1)
  for(i in min_horizon:max_horizon){
    size_params[i + 1] <- fit_nb(x = to_add_already_observed[, i + 1], 
                                 mu = expectation_to_add_already_observed[, i + 1] + 0.1)
    # plus 0.1 to avoid ill-defined NB
  }
  
  
  # generate actual nowcast in standard format:
  mu <- expectation_to_add[nrow(expectation_to_add), ]
  forecast_date <- as.Date(tail(observed$date, 1))
  quantile_levels <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
  df_all <- NULL
  
  # run through horizons:
  for(d in min_horizon:max_horizon){
    # by how mch do we need to shift quantiles upwards?
    already_observed <- sum(matr_observed[nrow(matr_observed) - (d), ], na.rm = TRUE)
    
    # data frame for expecations:
    df_mean <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - 7*d,
                          target = paste0(-d, " week ahead ", target_type),
                          type = "mean",
                          quantile = NA,
                          value = round(mu[d + 1] + already_observed),
                          pathogen = pathogen)
    
    # obtain quantiles:
    qtls0 <- qnbinom(quantile_levels, 
                     size = size_params[d + 1], mu = mu[d + 1])
    # shift them up by already oberved values
    qtls <- qtls0 + already_observed
    # data.frame for quantiles:
    df_qtls <- data.frame(location = location,
                          age_group = age_group,
                          forecast_date = forecast_date,
                          target_end_date = forecast_date - 7*d,
                          target = paste0(-d, " week ahead ", target_type),
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
  return(df_all)
}


######## Plotting functions:

# get the subset of a forecast file needed for plotting:
subset_forecasts_for_plot <- function(forecasts, forecast_date = NULL, target_type, horizon, location, age_group, type = NULL){
  check_target <- if(is.null(horizon)){
    grepl(target_type, forecasts$target)
  } else{
    grepl(horizon, forecasts$target) & grepl(target_type, forecasts$target)
  }
  check_forecast_date <- if(is.null(forecast_date)) TRUE else forecasts$forecast_date == forecast_date
  
  forecasts <- forecasts[check_target &
                           check_forecast_date &
                           forecasts$location == location &
                           forecasts$age_group == age_group, ]
  if(!is.null(type)) forecasts <- forecasts[forecasts$type == type, ]
  return(forecasts)
}

determine_ylim <- function(forecasts, forecast_date = NULL, target_type, horizon, location, age_group, truth, start_at_zero = TRUE){
  truth <- subset(truth, date >= forecast_date - 28)
  forecasts <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                         target_type = target_type, horizon = horizon, location = location, age_group = age_group)
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
draw_prediction_band <- function(forecasts, forecast_date = NULL, target_type, horizon,
                                 location, age_group, coverage, col = "lightgrey"){
  if(!coverage %in% c(1:9/10, 0.95, 0.98)) stop("Coverage needs to be from 0.1, 0.2, ..., 0.9, 0.95, 0.98")
  
  forecasts <- subset_forecasts_for_plot(forecasts  =forecasts, forecast_date = forecast_date,
                                         target_type = target_type, horizon = horizon, location = location,
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
draw_fanplot <- function(forecasts, target_type, forecast_date, horizon, location, age_group, levels_coverage = c(1:9/10, 0.95, 0.98),
                         cols = colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]){
  for(i in rev(seq_along(levels_coverage))){
    draw_prediction_band(forecasts = forecasts,
                         target_type = target_type,
                         horizon = horizon,
                         forecast_date = forecast_date,
                         location = location,
                         age_group = age_group,
                         coverage = levels_coverage[i],
                         col = cols[i])
  }
}

# add points for point forecasts:
draw_points <- function(forecasts, target_type, horizon, forecast_date, location, age_group, col = "deepskyblue4"){
  forecasts <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                         target_type = target_type, horizon = horizon, location = location,
                                         age_group = age_group, type = "quantile")
  points <- subset(forecasts, quantile == 0.5)
  # if no medians available: use means:
  if(nrow(points) == 0){
    points <- subset_forecasts_for_plot(forecasts = forecasts, forecast_date = forecast_date,
                                        target_type = target_type, horizon = horizon, location = location,
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
# target_type: "inc death" or "cum death"
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
                          target_type = "inc hosp",
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
                          ylab = target_type,
                          start_at_zero = TRUE){
  
  if(is.null(horizon) & is.null(forecast_date)) stop("Exactly one out of horizon and forecast_date needs to be specified")
  
  forecasts <- subset(forecasts, target_end_date >= start - 7 & target_end_date <= end)
  forecasts <- forecasts[order(forecasts$target_end_date), ]
  # truth <- truth[truth$date >= start & truth$location == location, ]
  xlim <- c(start, end)
  
  if(is.null(ylim)) ylim <- determine_ylim(forecasts = forecasts, forecast_date = forecast_date,
                                           target_type = target_type, horizon = horizon,
                                           location = location, age_group = age_group,
                                           truth = truth, start_at_zero = start_at_zero)
  
  empty_plot(xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  if(!is.null(forecast_date)) abline(v = forecast_date, lty = "dashed")
  draw_fanplot(forecasts = forecasts, target_type = target_type,
               horizon = horizon, forecast_date = forecast_date,
               location = location, age_group = age_group, levels_coverage = levels_coverage,
               cols = cols_intervals)
  draw_points(forecasts = forecasts, target_type = target_type,
              horizon = horizon, forecast_date = forecast_date,
              location = location, age_group = age_group, col = col_point)
  draw_truths(truth = truth, location = location)
}