# function to convert the the reporting triangle to a long format that can be transformed further
# for use by epinowcast
# Arguments:
# data: the data frame as passed withinpreprocess_respinow
# unit: 1 for daily data, 7 for weekly data
convert_reptri_to_long = function(data, unit = 7){
  
  # bring date to first column:
  date_col = data %>% select(date)
  data <- data |> select(-date)
  data <- bind_cols(date_col, data)
  
  # remove columns year and week (column date remains)
  data = data |> subset(select = -c(year, week))
  
  # adapt colnames, naming columns for delays simply 0:N
  colnames(data) <- c("reference_date", "location", "age_group", seq(0, ncol(data) - 4))
  # transform to long format:
  data <- data |>  pivot_longer(cols = - c("reference_date", "location", "age_group"),
                                names_to = "delay", values_to = "new_confirm") |> as.data.table()
  # ensure reference_date is a date:
  data$reference_date <- data$reference_date |> as.Date()
  # transform delay (which is currently a string) to integer:
  data$delay <- data$delay |> as.integer()
  # compute reporting date:
  data$report_date <- data$reference_date + data$delay * unit
  # sort rows:
  data <- setorder(data,reference_date, location, age_group, report_date)
  # add column with cumulatives
  data[, confirm := cumsum(new_confirm), by = c("reference_date", "location", "age_group")]
  # remove rows with missing values:
  return(data |> na.omit())
}

# subset data from Hub format to relevant dates and subgroups
# moving_window: number of weeks to include in fitting
# locations: locations to subset to
restrict_reptri <- function(data, forecast_date, moving_window = 10, max_d = NULL, locations = NULL, age_groups = NULL){
  # subset to relevant strata:
  if(!is.null(locations)){
    data <- subset(data, location %in% locations)
  }
  if(!is.null(age_groups)){
    data <- subset(data, age_group %in% age_groups)
  }
  if(!is.null(max_d)){
    data <- subset(data, delay <= max_d)
  }
  
  
  # restrict data to relevant dates:
  retro_resp <- data |> 
    enw_filter_report_dates(latest_date = forecast_date) |> # use only data reported up to <date> - ensures function can be used retrospectively
    enw_filter_reference_dates(include_days = moving_window * 7) # use only data from the moving window
  earliest_date <- min(retro_resp$reference_date) # identify first date

  return(retro_resp)
}

# turn into a daily format usable by epinowcast:
pretend_daily_reptri <- function(data, max_d, earliest_date = NULL){
  # determine an earliest date if not provided
  if(is.null(earliest_date)){
    earliest_date <- min(data$reference_date) # identify first date
  }
  
  # transform to daily data so epinowcasts can handle the data
  data$reference_date <- earliest_date + (data$reference_date - earliest_date) / 7
  data$report_date <- data$reference_date + data$delay
  
  # data pre-processing implemented in epinowcast
  data |> enw_preprocess_data(by = c("location","age_group"), max_delay = max_d)
}

# function to re-format output to our standard format:
reformat_nowcast_to_weekly <- function(nowcast, forecast_date, earliest_date, pathogen = "", data_source = "",
                                       probs = c(0.025,0.1,0.25,0.75,0.9,0.975), max_horizon = 4){
  # get quantiles at relevant levels and format outcome:
  nowcast <- nowcast |> summary(type = "nowcast", probs = probs) |> 
    subset(select = c("reference_date","age_group","location","mean","median","q2.5","q10","q25","q75","q90","q97.5"))
  # move back to weekly time indexing:
  nowcast$reference_date <- as.Date(earliest_date) + (as.Date(nowcast$reference_date) - as.Date(earliest_date)) * 7
  # store forecast date:
  nowcast$forecast_date <- forecast_date
  # compute horizon:
  nowcast$target <- floor((nowcast$forecast_date - nowcast$reference_date) / 7)
  # subset to four weeks back:
  nowcast <- nowcast |> subset(target <= max_horizon) |> 
    rename(target_end_date = reference_date, "0.5" = median, "0.025" = q2.5, "0.1" = q10, "0.25" = q25, "0.75" = q75, "0.9" = q90, "0.975" = q97.5) |>
    melt(id = c("target_end_date","age_group","location", "forecast_date","target"), variable.name = "type", value.name = "value")
  # create additional necessary columns:
  suppressWarnings(nowcast$quantile <- as.numeric(as.character(nowcast$type)))
  nowcast$type <- ifelse(is.na(nowcast$quantile),"mean","quantile")
  nowcast$horizon <- -1 * as.numeric(nowcast$target)
  nowcast$pathogen <- pathogen
  nowcast$data_source <- data_source
  # re-order columns:
  nowcast <- nowcast[, c("location", "age_group", "forecast_date", "target_end_date", "horizon",
                         "type", "quantile", "value", "pathogen")]
  
  return(nowcast)
}
