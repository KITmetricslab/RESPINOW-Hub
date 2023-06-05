source("code/data_utils.R")

pathogen <- "seasonal_influenza"

df <- load_submissions(pathogen, retrospective = TRUE, add_truth = FALSE) %>%
  filter(!model %in% c("MeanEnsemble", "MedianEnsemble"))

### MeanEnsemble

df_ensemble <- df %>%
  group_by(location, age_group, forecast_date, target_end_date, target, type, quantile, pathogen) %>%
  summarize(value = mean(value))

forecast_dates <- unique(df_ensemble$forecast_date)

for (f in as.list(forecast_dates)){
  print(f)
  filename <- str_glue("submissions/retrospective/{pathogen}/MeanEnsemble/{f}-MeanEnsemble.csv")
  df_temp <- df_ensemble %>% filter(forecast_date == f)
  write_csv(df_temp, filename)
}


### MedianEnsemble

df_ensemble2 <- df %>%
  group_by(location, age_group, forecast_date, target_end_date, target, type, quantile, pathogen) %>%
  summarize(value = median(value))

forecast_dates <- unique(df_ensemble2$forecast_date)

for (f in as.list(forecast_dates)){
  print(f)
  filename <- str_glue("submissions/retrospective/{pathogen}/MedianEnsemble/{f}-MedianEnsemble.csv")
  df_temp <- df_ensemble2 %>% filter(forecast_date == f)
  write_csv(df_temp, filename)
}

