library(tidyverse)
library(jsonlite)

load_file_as_of <- function(owner, repo, filepath, as_of) {
  # retrieve all commits on the given date
  commits <- fromJSON(
    paste0(
      "https://api.github.com/repos/", owner, "/", repo, "/commits?path=", filepath,
      # "&since=", as.Date(as_of) - tolerance,
      "&until=", as_of
    ),
    simplifyDataFrame = TRUE, flatten = TRUE
  )

  # get sha of latest commit before or on the given date
  sha <- commits %>%
    filter(commit.author.date == max(commit.author.date)) %>%
    pull(sha)

  # load the corresponding data
  read_csv(paste0(
    "https://raw.githubusercontent.com/", owner, "/", repo, "/",
    sha,
    "/", filepath
  ), show_col_types = FALSE)
}


# Load raw reporting triangle
load_survstat <- function(pathogen, as_of = Sys.Date()) {
  filepath <- paste0("data/Survstat/", pathogen, "_reporting_triangle_survstat_preprocessed.csv")
  load_file_as_of("KITmetricslab", "RESPINOW-Data", filepath, as_of = as.Date(as_of) + 1)
}

# df <- load_survstat("seasonal_influenza", "2023-01-15")


load_truth <- function(pathogen){
  filepath <- paste0("data/Survstat/", pathogen, "_reporting_triangle_survstat_preprocessed.csv")
  df_truth <- load_file_as_of("KITmetricslab", "RESPINOW-Data", filepath, as_of = Sys.Date())

  df_truth %>%
    rowwise() %>%
    mutate(truth = sum(across(value_0w:value_4w))) %>%
    select(-c(value_0w:`value_>10w`, week, year))
}


load_frozen_truth <- function(pathogen, delay = 0){
  filepath <- paste0("data/Survstat/", pathogen, "_reporting_triangle_survstat_preprocessed.csv")
  df_truth <- load_file_as_of("KITmetricslab", "RESPINOW-Data", filepath, as_of = Sys.Date())

  df_truth %>%
    rowwise() %>%
    mutate(frozen_truth = sum(across(value_0w:paste0("value_", delay, "w")))) %>%
    select(-c(value_0w:`value_>10w`, week, year))
}
