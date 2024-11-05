library(tidyverse)

path_submissions <- 'submissions/icosari/sari/'

load_member_models <- function(models, forecast_date){
  df <- data.frame()
  for (model in models) {
    filepath <- paste0(path_submissions, model, '/', forecast_date, '-icosari-sari-', model, '.csv')
    df_temp <- read_csv(filepath, show_col_types = FALSE) %>%
      mutate(model = model) %>%
      filter(type == 'quantile')

    df <- rbind(df, df_temp)
  }
  return(df)
}

compute_ensemble <- function(models, forecast_date){
  df <- load_member_models(models, forecast_date)

  df_ensemble <- df %>%
    group_by(location, age_group, forecast_date, target_end_date,
             horizon, type, quantile) %>%
    summarize(value = mean(value), .groups = "drop") %>%
    mutate(age_group = factor(age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))) %>%
    arrange(age_group)

  return(df_ensemble)
}
