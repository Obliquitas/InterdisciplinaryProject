library(tidyr)
library(dplyr)
library(TSclust)
library(purrr)

# Load smoothed annotations
ztc_smoothed_annotations <- readRDS("data/ztc_smoothed_annotations.rds")


# Calculate mean emotion metrics for each grouping of session, task and subject


mean_value_per_subject <- function(data) {
  pair <- data %>% select(bundle) %>% distinct()
  subject_a <- substr(pair[[1]][1], 1, 2)
  subject_b <- substr(pair[[1]][1], 3, 4)
  prediction_valence_a <- data %>% filter(subject == subject_a) %>% select(smoothed_valence) %>% unlist()
  prediction_valence_b <- data %>% filter(subject == subject_b) %>% select(smoothed_valence) %>% unlist()
  mean_valence_a <- mean(discard(prediction_valence_a, is.na))
  mean_valence_b <- mean(discard(prediction_valence_b, is.na))

  prediction_arousal_a <- data %>% filter(subject == subject_a) %>% select(smoothed_arousal) %>% unlist()
  prediction_arousal_b <- data %>% filter(subject == subject_b) %>% select(smoothed_arousal) %>% unlist()
  mean_arousal_a <- mean(discard(prediction_arousal_a, is.na))
  mean_arousal_b <- mean(discard(prediction_arousal_b, is.na))

  prediction_dominance_a <- data %>% filter(subject == subject_a) %>% select(smoothed_dominance) %>% unlist()
  prediction_dominance_b <- data %>% filter(subject == subject_b) %>% select(smoothed_dominance) %>% unlist()
  mean_dominance_a <- mean(discard(prediction_dominance_a, is.na))
  mean_dominance_b <- mean(discard(prediction_dominance_b, is.na))

  return(tibble(mean_valence_a = mean_valence_a,
                mean_valence_b = mean_valence_b,
                mean_arousal_a = mean_arousal_a,
                mean_arousal_b = mean_arousal_b,
                mean_dominance_a = mean_dominance_a,
                mean_dominance_b = mean_dominance_b))
}

ztc_mean_scores <- ztc_smoothed_annotations %>%
  group_by(session, task) %>%
  group_modify(~mean_value_per_subject(.))

# Calculate trajectory of emotion metrics for each grouping of session, task and subject

trajectory_per_subject <- function(data){
  pair <- data %>% select(bundle) %>% distinct()
  subject_a <- substr(pair[[1]][1], 1, 2)
  subject_b <- substr(pair[[1]][1], 3, 4)

  start_a <- data %>% filter(subject == subject_a) %>% select(start) %>% unlist()
  start_b <- data %>% filter(subject == subject_b) %>% select(start) %>% unlist()

  prediction_valence_a <- data %>% filter(subject == subject_a) %>% select(smoothed_valence) %>% unlist()
  prediction_valence_b <- data %>% filter(subject == subject_b) %>% select(smoothed_valence) %>% unlist()
  trajectory_valence_a <- coef(lm(prediction_valence_a ~ start_a))[2]
  trajectory_valence_b <- coef(lm(prediction_valence_b ~ start_b))[2]

  prediction_arousal_a <- data %>% filter(subject == subject_a) %>% select(smoothed_arousal) %>% unlist()
  prediction_arousal_b <- data %>% filter(subject == subject_b) %>% select(smoothed_arousal) %>% unlist()
  trajectory_arousal_a <- coef(lm(prediction_arousal_a ~ start_a))[2]
  trajectory_arousal_b <- coef(lm(prediction_arousal_b ~ start_b))[2]

  prediction_dominance_a <- data %>% filter(subject == subject_a) %>% select(smoothed_dominance) %>% unlist()
  prediction_dominance_b <- data %>% filter(subject == subject_b) %>% select(smoothed_dominance) %>% unlist()
  trajectory_dominance_a <- coef(lm(prediction_dominance_a ~ start_a))[2]
  trajectory_dominance_b <- coef(lm(prediction_dominance_b ~ start_b))[2]

  return(tibble(valence_alignment = trajectory_valence_a - trajectory_valence_b,
                arousal_alignment = trajectory_arousal_a - trajectory_arousal_b,
                dominance_alignment = trajectory_dominance_a - trajectory_dominance_b))
}


ztc_trajectory_scores <- ztc_smoothed_annotations %>%
  group_by(session, task) %>%
  group_modify(~trajectory_per_subject(.))

alignedness <- function(data) {
  pair <- data %>% select(bundle) %>% distinct()
  subject_a <- substr(pair[[1]][1], 1, 2)
  subject_b <- substr(pair[[1]][1], 3, 4)
  prediction_valence_a <- data %>% filter(subject == subject_a) %>% select(smoothed_valence) %>% unlist()
  prediction_valence_b <- data %>% filter(subject == subject_b) %>% select(smoothed_valence) %>% unlist()
  alignedness_valence <- diss.DTWARP(discard(prediction_valence_a, is.na), discard(prediction_valence_b, is.na))

  prediction_arousal_a <- data %>% filter(subject == subject_a) %>% select(smoothed_arousal) %>% unlist()
  prediction_arousal_b <- data %>% filter(subject == subject_b) %>% select(smoothed_arousal) %>% unlist()
  alignedness_arousal <- diss.DTWARP(discard(prediction_arousal_a, is.na), discard(prediction_arousal_b, is.na))

  prediction_dominance_a <- data %>% filter(subject == subject_a) %>% select(smoothed_dominance) %>% unlist()
  prediction_dominance_b <- data %>% filter(subject == subject_b) %>% select(smoothed_dominance) %>% unlist()
  alignedness_dominance <- diss.DTWARP(discard(prediction_dominance_a, is.na), discard(prediction_dominance_b, is.na))

  return(tibble(alignedness_valence = alignedness_valence,
                alignedness_arousal = alignedness_arousal,
                alignedness_dominance = alignedness_dominance))
}


# Calculate the distance between the smoothed values of two subjects using DTW
ztc_dtw_alignedness <- ztc_smoothed_annotations %>%
  group_by(session, task) %>%
  group_modify(~alignedness(.))

# Merge the mean scores, trajectory scores and alignedness scores
ztc_emotion_metrics <- ztc_mean_scores %>%
  inner_join(ztc_trajectory_scores, by = c("session", "task")) %>%
  inner_join(ztc_dtw_alignedness, by = c("session", "task")) %>%
  ungroup()
#Save the emotion metrics
saveRDS(ztc_emotion_metrics, "data/ztc_emotion_metrics.rds")