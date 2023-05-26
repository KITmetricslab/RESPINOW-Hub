source("code/data_utils.R")
source("code/load_truth.R")
source("code/scoring_functions.R")

plot_wis <- function(df, level = "national"){
  df_temp <- filter_data(df, level = {{ level }})
  df_temp <- compute_wis(df_temp)

  scores <- df_temp %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")

  ggplot(scores, aes(x = model)) +
    geom_point(data = df_temp, aes(x = model, y = ae, fill = model), shape = 23, size = 1) +
    geom_bar(aes(y = value), fill = "white", stat = "sum", show.legend = FALSE) + # so you can't see through bars
    geom_bar(aes(y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df_temp,
      aes(y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.25, "lines"),
      size = 8 / .pt,
      label.padding = unit(0.15, "lines")
    ) +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    guides(color = "none", fill = "none") +
    labs(
      title = paste(PATHOGENS[unique(df$pathogen)], tolower(TITLES[level]), sep = ": "),
      x = NULL,
      y = "Mean WIS / AE",
      color = "Model",
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
}

df <- load_submissions("seasonal_influenza", retrospective = TRUE, add_truth = TRUE)

plot_wis(df, "national")
plot_wis(df, "states")
plot_wis(df, "age")
