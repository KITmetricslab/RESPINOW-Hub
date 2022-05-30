library(tidyverse)

plot_truth <- function(disease = "Seasonal_Influenza", kind = "age"){
  df <- read_csv(paste0("data/truth/latest_truth_", tolower(disease), ".csv"), show_col_types = FALSE) %>% 
    filter(date >= "2015-01-01")
  df$age_group <- factor(df$age_group, levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+"))

  if (kind == 'age'){
    df %>% 
      filter(location == "DE") %>% 
      ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_wrap('age_group', scales = "free_y") +
      labs(x = NULL, y = "Cases", title = str_replace(disease, "_", " "))
  }
  
  else if (kind == "states"){
    df %>% 
      filter(age_group == "00+") %>% 
      ggplot(aes(x = date, y = value)) +
      geom_line() +
      facet_wrap('location', scales = "free_y") +
      labs(x = NULL, y = "Cases", title = str_replace(disease, "_", " "))
  }
}

plot_truth(disease = "Seasonal_Influenza", kind = "age")
plot_truth(disease = "Seasonal_Influenza", kind = "states")

plot_truth(disease = "RSV_Infection", kind = "age")
plot_truth(disease = "RSV_Infection", kind = "states")

plot_truth(disease = "Pneumococcal_Disease", kind = "age")
plot_truth(disease = "Pneumococcal_Disease", kind = "states")
