library(tidyr)
library(dplyr)
library(ggplot2)


# Load the data
ztc_emotion_metrics_end_times <- readRDS("data/ztc_emotion_metrics_end_times.rds")

# Gather the data into a long format
long_data <- ztc_emotion_metrics_end_times %>%
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
  ggsave(filename = paste0("annotations_plot_", id, ".png"), plot = p)
}