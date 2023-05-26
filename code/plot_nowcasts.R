source("code/load_truth.R")
Sys.setlocale("LC_ALL", "C")

plot_nowcasts <- function(pathogen, model, horizon, retrospective,
                          location = "DE", age_group = "00+", export = FALSE) {
  target <- str_glue("{ifelse(horizon == '-0', '0', -horizon)} week ahead inc hosp")
  path <- str_glue("submissions/{ifelse(retrospective, 'retrospective/', '')}{pathogen}/{model}/")

  # load final and frozen truth
  df_truth <- load_truth(pathogen)
  df_frozen_truth <- load_frozen_truth(pathogen, horizon)

  # load all submissions into one dataframe in long format
  df <- list.files(path, pattern = "*.csv", full.names = TRUE) %>%
    map_df(~ read_csv(., show_col_types = FALSE, progress = FALSE))

  df <- df %>%
    left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date")) %>%
    left_join(df_frozen_truth, by = c("location", "age_group", "target_end_date" = "date"))

  df <- df %>%
    filter(
      target == {{ target }},
      type == "quantile",
      location == {{ location }},
      age_group == {{ age_group }}
    ) %>%
    pivot_wider(names_from = quantile, names_prefix = "quantile_")

  ALPHAS <- setNames(c(0.7, 0.4), c("50%", "95%"))
  LINE_COLORS <- setNames(c("firebrick3", "gray"), c("Final", "At time of nowcast"))

  p <- ggplot(df, aes(x = target_end_date)) +
    geom_line(aes(x = target_end_date, y = frozen_truth, color = "At time of nowcast"), linetype = "solid") +
    geom_line(aes(x = target_end_date, y = quantile_0.5)) +
    geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
      fill = "skyblue3"
    ) +
    geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
      fill = "skyblue3"
    ) +
    geom_line(aes(x = target_end_date, y = truth, color = "Final")) +
    labs(x = NULL, y = "7-day case incidence", title = paste("Horizon:", horizon, "weeks")) +
    scale_alpha_manual(
      name = "Nowcasts with \nprediction intervals:", values = ALPHAS,
      guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
    ) +
    scale_color_manual(
      name = "Truth", values = LINE_COLORS,
      guide = guide_legend(order = 1, title.position = "top", title.hjust = 0)
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.65, "lines"),
      strip.text = element_text(size = 8, margin = margin(b = 2, t = 2)),
      axis.title.y = element_text(size = 8),
      axis.text = element_text(size = 7),
      axis.ticks = element_line(colour = "black", size = 0.25),
      panel.grid.major = element_line(size = 0.15),
      panel.grid.minor = element_line(size = 0.1),
      plot.margin = unit(c(1, 1.5, 0, 1.5), "pt"),
      legend.margin = margin(0, 0, 0, 5),
      legend.box.spacing = unit(0, "pt"),
      legend.background = element_rect(fill = "transparent")
    )

  if (export) {
  ggsave(str_glue("figures/nowcasts_{pathogen}_{model}_{horizon}w.pdf"), p,
       width = 160, height = 100, unit = "mm", device = "pdf")
  }

  return(p)
}

plot_nowcasts("seasonal_influenza", "KIT-KEW", 2, retrospective = TRUE, export = TRUE)
