source("code/data_utils.R")
source("code/load_truth.R")

plot_coverage <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))

  ggplot(coverage_df, aes(x = model)) +
    expand_limits(y = 1) +
    geom_col(aes(y = c95, fill = model, alpha = "95%")) +
    geom_col(aes(y = c50, fill = model, alpha = "50%")) +
    geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      title = paste(PATHOGENS[unique(df$pathogen)], tolower(TITLES[level]), sep = ": "),
      x = NULL,
      y = "Empirical coverage",
      color = "Model",
      alpha = "Prediction interval"
    ) +
    coord_flip() +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
}


plot_coverage_by_horizon <- function(df, level = "national") {
  df <- filter_data(df, type = "quantile", level = level) %>%
    mutate(horizon = as.numeric(str_extract(target, "-?\\d+")))

  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_")

  df_wide <- df_wide %>%
    mutate(
      c50 = (truth >= quantile_0.25 & truth <= quantile_0.75),
      c95 = (truth >= quantile_0.025 & truth <= quantile_0.975)
    )

  coverage_df <- df_wide %>%
    group_by(model, horizon) %>%
    summarize(
      c50 = mean(c50, na.rm = TRUE),
      c95 = mean(c95, na.rm = TRUE)
    )

  coverage_long <- coverage_df %>%
    pivot_longer(cols = c(c50, c95), names_to = "quantile") %>%
    mutate(
      quantile_label = paste0(str_sub(quantile, 2, 3), "% prediction interval"),
      quantile_level = as.numeric(paste0("0.", str_sub(quantile, 2, 3)))
    )

  nominal_levels <- data.frame(
    quantile_label = c("50% prediction interval", "95% prediction interval"),
    quantile_level = c(0.5, 0.95)
  )


  ggplot(coverage_long, aes(x = horizon, y = value, color = model)) +
    facet_wrap("quantile_label") +
    geom_line() +
    theme_bw() +
    geom_hline(data = nominal_levels, aes(yintercept = quantile_level), linetype = "dashed") +
    scale_x_continuous(
      breaks = 0:4 * -1,
    ) +
    labs(
      title = paste(PATHOGENS[unique(df$pathogen)], tolower(TITLES[level]), sep = ": "),
      x = "Horizon (weeks)",
      y = "Empirical coverage",
      color = "Model"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}


df <- load_submissions("seasonal_influenza", retrospective = TRUE, add_truth = TRUE)

plot_coverage(df, "national")
plot_coverage(df, "states")
plot_coverage(df, "age")

plot_coverage_by_horizon(df, "national")
plot_coverage_by_horizon(df, "states")
plot_coverage_by_horizon(df, "age")
