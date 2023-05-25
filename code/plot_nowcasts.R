source("code/load_data.R")
Sys.setlocale("LC_ALL", "C")

MODEL <- "KIT-KEW"

PATH <- str_glue("submissions/retrospective/{MODEL}/")
files <- list.files(PATH, pattern = "*.csv", recursive = TRUE)

# load all submissions into one dataframe in long format
df <- data.frame()
for (f in files) {
  df_temp <- read_csv(paste0(PATH, "/", f),
                      show_col_types = FALSE, progress = FALSE
  )
  df <- bind_rows(df, df_temp)
}

HORIZON <- 0

df_truth <- load_truth("seasonal_influenza")
df_frozen_truth <- load_frozen_truth("seasonal_influenza", HORIZON)

df <- df %>%
  left_join(df_truth, by = c("location", "age_group", "target_end_date" = "date")) %>%
  left_join(df_frozen_truth, by = c("location", "age_group", "target_end_date" = "date"))

if (HORIZON == 0) {
  t <- "0 week ahead inc hosp"
} else {
  t <- paste0("-", HORIZON, " week ahead inc hosp")
}

df1 <- df %>%
  filter(
    target == t,
    type == "quantile",
    location == "DE",
    age_group == "00+"#,
    #target_end_date >= "2023-02-01"
  ) %>%
  pivot_wider(names_from = quantile, names_prefix = "quantile_")

alphas <- setNames(c(0.7, 0.4), c("50%", "95%"))
line_colors <- setNames(c("firebrick3", "gray"), c("Final", "At time of nowcast"))

ggplot(df1, aes(x = target_end_date)) +
  geom_line(aes(x = target_end_date, y = frozen_truth, color = "At time of nowcast"), linetype = "solid") +
  geom_line(aes(x = target_end_date, y = quantile_0.5)) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.025, ymax = quantile_0.975, alpha = "95%"),
              fill = "skyblue3"
  ) +
  geom_ribbon(aes(x = target_end_date, ymin = quantile_0.25, ymax = quantile_0.75, alpha = "50%"),
              fill = "skyblue3"
  )+
  geom_line(aes(x = target_end_date, y = truth, color = "Final")) +
  labs(x = NULL, y = "7-day case incidence", title = paste("Horizon:", HORIZON, "weeks")) +
  scale_alpha_manual(
    name = "Nowcasts with \nprediction intervals:", values = alphas,
    guide = guide_legend(order = 2, title.position = "top", title.hjust = 0)
  ) +
  scale_color_manual(
    name = "Truth", values = line_colors,
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


ggsave(str_glue("figures/nowcasts_{MODEL}_{HORIZON}w.pdf"), width = 164, height = 100, unit = "mm", device = "pdf")

