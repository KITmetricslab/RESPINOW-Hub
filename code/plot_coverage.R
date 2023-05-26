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


df <- load_submissions("seasonal_influenza", retrospective = TRUE, add_truth = TRUE)

plot_coverage(df, "national")
plot_coverage(df, "states")
plot_coverage(df, "age")
