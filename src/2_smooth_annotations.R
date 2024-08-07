library(tidyr)
library(dplyr)
library(mgcv)

# Load annotated utterings
ztc_annotated_utterings <- readRDS("data/ztc_annotated_utterings.rds")

#Smooth the annotated values by using a function that fits a gam 
#model to the specified variable and returns the smoothed values

smooth_annotated_values <- function(data) {
  pair <- data %>% select(bundle) %>% distinct()
  subject_a <- substr(pair[[1]][1], 1, 2)
  subject_b <- substr(pair[[1]][1], 3, 4)

  data_a <- data %>% filter(subject == subject_a)
  data_b <- data %>% filter(subject == subject_b)

  #If there are more than 5 observations, fit gam model else fit a linear model
  if (nrow(data_a) > 5) {
    valence_model_a <- gam(valence ~ s(start, k = 5), data = data_a)
    dominance_model_a <- gam(dominance ~ s(start, k = 5), data = data_a)
    arousal_model_a <- gam(arousal ~ s(start, k = 5), data = data_a)
  } else {
    valence_model_a <- lm(valence ~ start, data = data_a)
    dominance_model_a <- lm(dominance ~ start, data = data_a)
    arousal_model_a <- lm(arousal ~ start, data = data_a)
  }

  if (nrow(data_b) > 5) {
    valence_model_b <- gam(valence ~ s(start, k = 5), data = data_b)
    dominance_model_b <- gam(dominance ~ s(start, k = 5), data = data_b)
    arousal_model_b <- gam(arousal ~ s(start, k = 5), data = data_b)
  } else {
    valence_model_b <- lm(valence ~ start, data = data_b)
    dominance_model_b <- lm(dominance ~ start, data = data_b)
    arousal_model_b <- lm(arousal ~ start, data = data_b)
  }

  #Predict the smoothed values for combined start times
  valence_smoothed_values_a <- predict(valence_model_a, newdata = data)
  valence_smoothed_values_b <- predict(valence_model_b, newdata = data)

  dominance_smoothed_values_a <- predict(dominance_model_a, newdata = data)
  dominance_smoothed_values_b <- predict(dominance_model_b, newdata = data)

  arousal_smoothed_values_a <- predict(arousal_model_a, newdata = data)
  arousal_smoothed_values_b <- predict(arousal_model_b, newdata = data)

  start <- rep(data$start, 2)
  end <- rep(data$end, 2)
  # Repeat subject_a and subject_b for the number of rows in data
  subjects <- rep(c(subject_a, subject_b), each = nrow(data))
  #Return smoothed values in long format

  return(tibble(start = start,
                end = end,
                subject = subjects,
                smoothed_valence = c(valence_smoothed_values_a,
                                     valence_smoothed_values_b),
                smoothed_dominance = c(dominance_smoothed_values_a,
                                       dominance_smoothed_values_b),
                smoothed_arousal = c(arousal_smoothed_values_a,
                                     arousal_smoothed_values_b)))
}

ztc_smoothed_annotations <- ztc_annotated_utterings %>%
  group_by(session, task) %>%
  group_modify(~smooth_annotated_values(.))

ztc_smoothed_annotations
# ungroup
ztc_smoothed_annotations <- ztc_smoothed_annotations %>% ungroup()

# Save the smoothed annotations
saveRDS(ztc_smoothed_annotations, file = "data/ztc_smoothed_annotations.rds")
