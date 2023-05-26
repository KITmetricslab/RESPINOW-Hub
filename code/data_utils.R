TITLES <- setNames(
  c("National level", "States", "Age groups"),
  c("national", "states", "age")
)

PATHOGENS <- setNames(
  c("Seasonal influenza", "RSV infection", "Pneumococcal disease"),
  c("seasonal_influenza", "rsv_infection", "pneumococcal_disease")
)


load_submissions <- function(pathogen, retrospective, add_truth) {
  path <- str_glue("submissions/{ifelse(retrospective, 'retrospective/', '')}{pathogen}/")
  models <- list.files(path)

  df <- data.frame()
  for (m in models) {
    df_temp <- list.files(str_glue("{path}/{m}/"), pattern = "*.csv", full.names = TRUE) %>%
      map_df(~ read_csv(., show_col_types = FALSE, progress = FALSE)) %>%
      mutate(model = m)
    df <- bind_rows(df, df_temp)
  }

  if (add_truth) {
    df_truth <- load_truth(pathogen)
    df <- df %>%
      left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date"))
  }
  return(df)
}


filter_data <- function(df, model, type, level = "national", short_horizons = FALSE) {
  if (!missing(type)) {
    df <- df %>% filter(type == !!type)
  }

  if (!missing(model)) {
    df <- df %>% filter(model == !!model)
  }

  if (short_horizons) {
    df <- df %>% filter(target %in% paste(0:7 * -1, "day ahead inc hosp"))
  }

  if (level == "national") {
    df <- df %>%
      filter(
        location == "DE",
        age_group == "00+"
      )
  } else if (level == "states") {
    df <- df %>%
      filter(
        location != "DE",
        model != "ILM"
      )
  } else if (level == "age") {
    df <- df %>%
      filter(
        location == "DE",
        age_group != "00+",
        model != "RKI"
      )
  }
  return(df)
}
