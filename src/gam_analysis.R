library(tidyr)
library(dplyr)
library(ggplot2)

gam_analyis <- function(data, name){
  long_data <- data %>%
    gather("id", "value", 3:14)
  # Loop through each unique id and create a plot for each one
  unique_ids <- unique(long_data$id)

  for (id in unique_ids) {
    # Filter data for the current id
    plot_data <- long_data %>% filter(id == !!id)

    p <- ggplot(plot_data, aes(x = value, y = end)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
      ggtitle(id) +
      theme_minimal()

    # Save the plot
    ggsave(filename = paste0("plots/", name, "_plot_", id, ".png"), plot = p)
  }
}