install.packages(c("dplyr", "readr", "purrr", "stringr"), dependencies = TRUE)

library(dplyr)
library(readr)
library(purrr)
library(stringr)

# target -> source mapping
target_map <- list(
  sari = "icosari",
  are  = "agi"
)

# target -> age group levels
age_group_map <- list(
  sari = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"),
  are  = c("00+", "00-04", "05-14", "15-34", "35-59", "60+")
)

load_member_models <- function(models, forecast_date, target) {

  source <- target_map[[target]]

  if (is.null(source)) {
    stop("target must be 'sari' or 'are'")
  }

  path_submissions <- paste0("submissions/", source, "/", target, "/")

  df <- data.frame()

  for (model in models) {

    filename <- paste0(
      forecast_date, "-", source, "-", target, "-", model, ".csv"
    )

    filepath <- paste0(path_submissions, model, "/", filename)

    df_temp <- read_csv(filepath, show_col_types = FALSE) %>%
      mutate(model = model) %>%
      filter(type == "quantile")

    df <- rbind(df, df_temp)
  }

  return(df)
}

compute_ensemble <- function(models, forecast_date, target){
  df <- load_member_models(models, forecast_date, target)

  df_ensemble <- df %>%
    group_by(location, age_group, forecast_date, target_end_date, horizon, type, quantile) %>%
    summarize(value = mean(value), .groups = "drop") %>%
    mutate(age_group = factor(age_group, levels = age_group_map[[target]])) %>%
    arrange(age_group)

  return(df_ensemble)
}

