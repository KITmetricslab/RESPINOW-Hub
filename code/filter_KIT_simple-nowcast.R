library(tidyverse)

files <- list.files("submissions/retrospective/pneumococcal_disease/KIT-simple_nowcast/", pattern = "*.csv", full.names = TRUE)

for (f in files) {
  df <- read_csv(f) %>%
    filter(pathogen == "pneumococcal_disease") %>%
    select(-any_of("data_source"))
  write_csv(df, f)
}
