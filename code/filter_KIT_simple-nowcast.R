library(tidyverse)

DISEASE <- "pneumococcal_disease"
DISEASE <- "seasonal_influenza"
DISEASE <- "rsv_infection"

files <- list.files(str_glue("submissions/retrospective/{DISEASE}/KIT-simple_nowcast/"), pattern = "*.csv", full.names = TRUE)

for (f in files) {
  df <- read_csv(f) %>%
    filter(pathogen == DISEASE) %>%
    select(-any_of("data_source"))
  write_csv(df, f)
}
