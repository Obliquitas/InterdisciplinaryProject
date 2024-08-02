library(tidyr)
library(dplyr)
library(ggplot2)


# Load the data
ztc_emotion_metrics_end_times <- readRDS("data/ztc_emotion_metrics_end_times.rds")

ztc_emotion_metrics_end_times %>% gather("id", "value", 3:14) %>%
  ggplot(aes(x = value, end)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  facet_wrap(~id, scales = "free") + 
  theme_minimal()
