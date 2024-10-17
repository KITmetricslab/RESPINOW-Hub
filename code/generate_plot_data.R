# generate plot data by date and pathogen

# set working directory
# current_path = rstudioapi::getActiveDocumentContext()$path # get path of this file
# setwd(dirname(current_path))
# setwd("..")

source("respinow_viz/functions.R")

# get data versions:
rep_tri <- read.csv("data/icosari/sari/reporting_triangle-icosari-sari.csv")
all_data_versions <- as.Date(unique(rep_tri$date)) + 4
df_data_versions <- data.frame(date = all_data_versions)
write.csv(df_data_versions, file = "respinow_viz/plot_data/other/list_data_versions.csv",
          row.names = FALSE, quote = FALSE)

# prepare summaries of available data sources, diseases and models:
data_sources <- list.dirs("data/", full.names = FALSE, recursive = FALSE)
all_data_sources <- all_diseases <- all_models <- character(0)
all_forecast_dates <- as.Date(character())

# run through data sources:
for(ds in data_sources){
  # get diseases:
  diseases <- list.dirs(paste0("data/", ds), full.names = FALSE, recursive = FALSE)
  for(di in diseases){
    all_diseases <- c(all_diseases, di)
    all_data_sources <- c(all_data_sources, ds)
    
    models <- list.dirs(paste0("submissions/", ds, "/", di), full.names = FALSE, recursive = FALSE)
    all_models <- c(all_models, models)
    
    for (mod in models) {
      files <- list.files(paste0("submissions/", ds, "/", di, "/", mod), full.names = FALSE, recursive = FALSE)
      files <- files[grepl(".csv", files)]
      forecast_dates <- date_from_filename(files)
      all_forecast_dates <- c(all_forecast_dates, forecast_dates[!forecast_dates %in% all_forecast_dates])
    }
  }
}
all_forecast_dates <- sort(all_forecast_dates)


# write out data sources + targets:
# this is created manually now:
df_targets <- data.frame(data_source = all_data_sources,
                          disease = all_diseases)
# write.csv(df_targets, file = "respinow_viz/plot_data/other/list_targets.csv",
#           row.names = FALSE, quote = FALSE)

# write out models:
all_models <- unique(all_models)
write.csv(data.frame(model = all_models), file = "respinow_viz/plot_data/other/list_models.csv",
          row.names = FALSE, quote = FALSE)

# write out forecast dates:
write.csv(data.frame(date = all_forecast_dates), file = "respinow_viz/plot_data/other/list_forecast_dates.csv",
          row.names = FALSE, quote = FALSE)



# collate nowcast data for plotting:
for(i in seq_along(all_forecast_dates)){
  all_forecasts <- NULL
  forecast_date <- all_forecast_dates[i]
  for(j in 1:nrow(df_targets)){
    ds <- df_targets$data_source[j]
    di <- df_targets$disease[j]
    for (mod in all_models) {
      fl <- paste0("submissions/", ds, "/", di, "/", mod, "/", 
                   forecast_date, "-", ds, "-", di, "-", mod, ".csv")
      if(file.exists(fl)){
        forecasts_temp <- read.csv(fl)
        forecasts_temp$X <- NULL # remove stray leading column with row names if present
        forecasts_temp <- subset(forecasts_temp, type == "mean" | quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))
        # handle means:
        forecasts_temp$quantile[forecasts_temp$type == "mean"] <- 999 # numeric place holder
        # remove type column
        forecasts_temp$type <- NULL
        # remove pathogen column (deprecated):
        forecasts_temp$pathogen <- NULL
        # bring to wide format
        forecasts_temp <- reshape(forecasts_temp, direction = "wide", timevar = "quantile",
                                  idvar = c("location", "age_group", "forecast_date", "horizon", "target_end_date"))
        # handle colnames (mean separately)
        colnames(forecasts_temp) <- gsub("value.999", "mean", colnames(forecasts_temp))
        colnames(forecasts_temp) <- gsub("value.", "q", colnames(forecasts_temp))
        # fill in median for mean if no mean has been submitted:
        if(!"mean" %in% colnames(forecasts_temp)){
          forecasts_temp$mean <- forecasts_temp$q0.5
        }
        # add column for model:
        forecasts_temp$model <- mod
        # additional columns:
        forecasts_temp$target_type <- sapply(strsplit(forecasts_temp$target, split = "ahead "), FUN = function(x) x[2])
        forecasts_temp$pathogen <- paste0(ds, "-", di)
        # re-order:
        forecasts_temp <- forecasts_temp[, c("model", "target_type", "forecast_date", "target_end_date",
                                             "location", "age_group", "pathogen",
                                             "mean", "q0.025", "q0.25", "q0.5", "q0.75", "q0.975")]
        # append:
        if(is.null(all_forecasts)){
          all_forecasts <- forecasts_temp
        }else{
          all_forecasts <- rbind(all_forecasts, forecasts_temp)
        }
      }
    }
  }
  write.csv(all_forecasts, file = paste0("respinow_viz/plot_data/submissions/", forecast_date, ".csv"),
            row.names = FALSE, quote = FALSE)
}

# generate csv on available plot data
available_plot_data <- list.files("respinow_viz/plot_data/submissions")
df_available_plot_data <- data.frame(file = available_plot_data)
write.csv(df_available_plot_data, file = "respinow_viz/plot_data/other/list_plot_data.csv",
          row.names = FALSE, quote = FALSE)



# copy reporting triangles
for(i in 1:nrow(df_targets)){
  ds <- df_targets$data_source[i]
  di <- df_targets$disease[i]
  
  fl_triangle <- paste0("reporting_triangle-", ds, "-", di, ".csv")
  file.copy(from = paste0("data/", ds, "/", di, "/", fl_triangle),
            to = paste0("respinow_viz/plot_data/truth/", fl_triangle), overwrite = TRUE)
  
  fl_latest <- paste0("latest_data-", ds, "-", di, ".csv")
  file.copy(from = paste0("data/", ds, "/", di, "/", fl_latest),
            to = paste0("respinow_viz/plot_data/truth/", fl_latest), overwrite = TRUE)
  
  # try to copy triangle for number of tests:
  fl_triangle_test <- paste0("reporting_triangle-", ds, "-", di, "-tests.csv")
  file.copy(from = paste0("data/", ds, "/", di, "/", fl_triangle_test),
            to = paste0("respinow_viz/plot_data/truth/", fl_triangle_test), overwrite = TRUE)
  
  fl_latest_test <- paste0("latest_data-", ds, "-", di, "-tests.csv")
  file.copy(from = paste0("data/", ds, "/", di, "/", fl_latest_test),
            to = paste0("respinow_viz/plot_data/truth/", fl_latest_test), overwrite = TRUE)
  
}


