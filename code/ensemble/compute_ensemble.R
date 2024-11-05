source("code/ensemble/ensemble_functions.R")

models <- c('KIT-hhh4', 'KIT-LightGBM', 'KIT-TSMixer')

common_dates <- models %>%
  map(~ list.files(paste0(path_submissions, .x), full.names = FALSE) %>%
        str_extract("^\\d{4}-\\d{2}-\\d{2}") %>%
        na.omit()) %>%
  reduce(intersect)

ensemble_dates <- list.files(paste0(path_submissions, 'KIT-MeanEnsemble'), full.names = FALSE) %>%
  str_extract("^\\d{4}-\\d{2}-\\d{2}") %>%
  na.omit()

# Find dates in common_dates that are missing from ensemble_dates
missing_dates <- setdiff(common_dates, ensemble_dates)

for (forecast_date in missing_dates){
  print(forecast_date)
  df_ensemble <- compute_ensemble(models, forecast_date)
  ensemble_path <- paste0(path_submissions, 'KIT-MeanEnsemble/', forecast_date, '-icosari-sari-KIT-MeanEnsemble.csv')
  write_csv(df_ensemble, ensemble_path)
}
