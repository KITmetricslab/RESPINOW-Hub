setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub/code")

# generate plot data by date and pathogen

files <- list.files("../data-processed_retrospective",
                    recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
files <- sapply(strsplit(files, split = "/"), FUN = function(x) x[2])

date_from_filename <- function(file) as.Date(substr(file, 1, 10))

forecast_dates <- date_from_filename(files)
df_forecast_dates <- data.frame(date = forecast_dates)
write.csv(df_forecast_dates, file = "../respinow_viz/plot_data/available_dates.csv")


diseases <- c("rsv_infection", "seasonal_influenza", "pneumococcal_disease")
models <- list.dirs("../data-processed_retrospective", recursive = FALSE, full.names = FALSE)


for(i in seq_along(forecast_dates)){
  d <- forecast_dates[i]
  all_forecasts <- NULL
  for(mod in models){
    forecasts_temp <- read.csv(paste0("../data-processed_retrospective/", mod, "/", d, "-", mod, ".csv"))
    forecasts_temp <- subset(forecasts_temp, type == "mean" | quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))
    forecasts_temp$type <- NULL
    forecasts_temp <- reshape(forecasts_temp, direction = "wide", timevar = "quantile",
                              idvar = c("location", "age_group", "forecast_date", "target", "pathogen", "target_end_date"))
    colnames(forecasts_temp)[grepl("value", colnames(forecasts_temp))] <- c("mean", paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975)))
    forecasts_temp$model <- mod
    forecasts_temp$target_type <- sapply(strsplit(forecasts_temp$target, split = "ahead "), FUN = function(x) x[2])
    forecasts_temp$retrospective <- FALSE
    forecasts_temp <- forecasts_temp[, c("model", "target_type", "forecast_date", "target_end_date",
                                         "location", "age_group", "pathogen",
                                         "mean", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975",
                                         "retrospective")]
    
    if(is.null(all_forecasts)){
      all_forecasts <- forecasts_temp
    }else{
      all_forecasts <- rbind(all_forecasts, forecasts_temp)
    }
  }
  write.csv(all_forecasts, file = paste0("../respinow_viz/plot_data/", d, "_forecast_data.csv"))
}

available_plot_data <- list.files("../respinow_viz/plot_data")
available_plot_data <- available_plot_data[grepl("forecast_data", available_plot_data)]
df_available_plot_data <- data.frame(file = available_plot_data)
write.csv(df_available_plot_data, file = "../respinow_viz/plot_data/list_plot_data.csv")
