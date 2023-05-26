files <- list.files("submissions/retrospective/seasonal_influenza/KIT-simple_nowcast/", pattern = "*.csv", full.names = TRUE)

for (f in files) {
  df <- read_csv(f) %>%
    filter(pathogen == "seasonal_influenza") %>%
    select(-any_of("data_source"))
  write_csv(df, f)
}
