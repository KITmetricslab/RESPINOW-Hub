# Quantile score
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}


# Compute squared error, absolute error or quantile score based on "type"
score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    return((prediction - observation)^2)
  } else if (type == "median") {
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    return(qs(prediction, observation, quantile))
  }
}


# Compute scores for each row in a dataframe
compute_scores <- function(df) {
  df <- df %>%
    rowwise() %>%
    mutate(
      score = score(value, truth, type, quantile),
      score = round(score, digits = 5)
    ) %>%
    select(-c(pathogen, value, truth))
}


# Compute WIS decomposition
compute_wis <- function(df, by_horizon = FALSE) {
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "pathogen", "retrospective", "truth")))

  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median,
      by = c(
        "location", "age_group", "forecast_date",
        "target_end_date", "target", "type", "model"
      )
    )

  df <- df %>%
    rowwise() %>%
    mutate(
      score = score(value, truth, type, quantile),
      spread = score(value, med, type, quantile),
      overprediction = ifelse(med > truth, score - spread, 0),
      underprediction = ifelse(med < truth, score - spread, 0),
      ae = score(med, truth, type, quantile)
    )

  if (by_horizon) {
    df <- df %>%
      mutate(horizon = as.numeric(str_extract(target, "-?\\d+"))) %>%
      group_by(model, horizon) %>%
      summarize(
        spread = mean(spread),
        overprediction = mean(overprediction),
        underprediction = mean(underprediction),
        score = mean(score),
        ae = mean(ae)
      )
  } else {
    df <- df %>%
      group_by(model) %>%
      summarize(
        spread = mean(spread),
        overprediction = mean(overprediction),
        underprediction = mean(underprediction),
        score = mean(score),
        ae = mean(ae)
      )
  }


  return(df)
}
