source("code/ensemble/ensemble_functions.R")

models <- c("KIT-hhh4", "KIT-LightGBM", "KIT-TSMixer")
ensemble_model <- "KIT-MeanEnsemble"

targets <- c("sari", "are")

for (target in targets) {

  source <- target_map[[target]]
  if (is.null(source)) stop("target must be 'sari' or 'are'")

  path_submissions <- paste0("submissions/", source, "/", target, "/")

  common_dates <- models %>%
    map(~ list.files(paste0(path_submissions, .x), full.names = FALSE) %>%
          str_extract("^\\d{4}-\\d{2}-\\d{2}") %>%
          na.omit()) %>%
    reduce(intersect)

  ensemble_dates <- list.files(paste0(path_submissions, ensemble_model), full.names = FALSE) %>%
    str_extract("^\\d{4}-\\d{2}-\\d{2}") %>%
    na.omit()

  missing_dates <- setdiff(common_dates, ensemble_dates)

  for (forecast_date in missing_dates) {
    print(paste(target, forecast_date))

    df_ensemble <- compute_ensemble(models, forecast_date, target)

    ensemble_path <- paste0(
      path_submissions, ensemble_model, "/",
      forecast_date, "-", source, "-", target, "-", ensemble_model, ".csv"
    )

    write_csv(df_ensemble, ensemble_path)
  }
}
