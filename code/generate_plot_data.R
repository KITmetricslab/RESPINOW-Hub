# generate plot data by date and pathogen

files <- list.files("data-processed_retrospective",
                    recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
files <- sapply(strsplit(files, split = "/"), FUN = function(x) x[2])
files <- files[grepl("20", files) & grepl(".csv", files)]

date_from_filename <- function(file) as.Date(substr(file, 1, 10))

forecast_dates <- date_from_filename(files)
df_forecast_dates <- data.frame(date = forecast_dates)
write.csv(df_forecast_dates, file = "respinow_viz/plot_data/available_dates.csv")

# path where the data repository can be found:
path_data <- "../RESPINOW/RESPINOW-Data"


models <- list.dirs("data-processed_retrospective", recursive = FALSE, full.names = FALSE)


for(i in seq_along(forecast_dates)){
  d <- forecast_dates[i]
  all_forecasts <- NULL
  for(mod in models){
    fl <- paste0("data-processed_retrospective/", mod, "/", d, "-", mod, ".csv")
    if(file.exists(fl)){
      forecasts_temp <- read.csv(fl)
      forecasts_temp$X <- NULL # remove stray leading column with row names if present
      forecasts_temp <- subset(forecasts_temp, type == "mean" | quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))
      # handle means:
      forecasts_temp$quantile[forecasts_temp$type == "mean"] <- NA
      # remove type column
      forecasts_temp$type <- NULL
      # bring to wide format
      forecasts_temp <- reshape(forecasts_temp, direction = "wide", timevar = "quantile",
                                idvar = c("location", "age_group", "forecast_date", "target", "pathogen", "target_end_date"))
      # handle colnames (mean separately)
      colnames(forecasts_temp) <- gsub("value.NA", "mean", colnames(forecasts_temp))
      colnames(forecasts_temp) <- gsub("value.", "q", colnames(forecasts_temp))
      # add column for model:
      forecasts_temp$model <- mod
      # additional columns:
      forecasts_temp$target_type <- sapply(strsplit(forecasts_temp$target, split = "ahead "), FUN = function(x) x[2])
      forecasts_temp$retrospective <- FALSE
      # re-order:
      forecasts_temp <- forecasts_temp[, c("model", "target_type", "forecast_date", "target_end_date",
                                           "location", "age_group", "pathogen",
                                           "mean", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975",
                                           "retrospective")]
      # append:
      if(is.null(all_forecasts)){
        all_forecasts <- forecasts_temp
      }else{
        all_forecasts <- rbind(all_forecasts, forecasts_temp)
      }
    }
  }
  write.csv(all_forecasts, file = paste0("respinow_viz/plot_data/", d, "_forecast_data.csv"))
}

# generate csv on available plot data
available_plot_data <- list.files("respinow_viz/plot_data")
available_plot_data <- available_plot_data[grepl("forecast_data", available_plot_data)]
df_available_plot_data <- data.frame(file = available_plot_data)
write.csv(df_available_plot_data, file = "respinow_viz/plot_data/list_plot_data.csv")

# copy reorting triangles
diseases_survstat <- c("rsv_infection", "seasonal_influenza", "pneumococcal_disease")

for(disease in diseases_survstat){
  file.copy(from = paste0("data/truth/truth_", disease, "_preprocessed.csv"),
            to = paste0("respinow_viz/plot_data/truth_", disease, "_survstat_preprocessed.csv"), overwrite = TRUE)
  file.copy(from = paste0("data/truth/latest_truth_", disease, ".csv"),
            to = paste0("respinow_viz/plot_data/latest_truth_", disease, "_survstat.csv"), overwrite = TRUE)
}

diseases_nrz <- c("influenza", "rsv")

for(disease in diseases_nrz){
  file.copy(from = paste0(path_data, "/data/NRZ/", disease, "_reporting_triangle_NRZ_preprocessed.csv"),
            to = paste0("../respinow_viz/plot_data/truth_", disease, "_nrz_preprocessed.csv"), overwrite = TRUE)
  file.copy(from = paste0(path_data, "/data/NRZ/", disease, "_latest_truth.csv"),
            to = paste0("../respinow_viz/plot_data/latest_truth_", disease, "_nrz.csv"), overwrite = TRUE)
}
