library(tidyr)
library(dplyr)
library(TSclust)
library(purrr)

# Load smoothed annotations
ztc_smoothed_annotations <- readRDS("data/ztc_smoothed_annotations.rds")


# Calculate mean emotion metrics for each grouping of session, task and subject


mean_value_per_subject <- function(data) {
  subjects <- data %>% select(subject) %>% distinct() %>% unlist()
  subject_a <- subjects[1]
  subject_b <- subjects[2]
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
  subjects <- data %>% select(subject) %>% distinct() %>% unlist()
  subject_a <- subjects[1]
  subject_b <- subjects[2]

  start <- data %>% filter(subject == subject_a) %>% select(start) %>% unlist()

  prediction_valence_a <- data %>% filter(subject == subject_a) %>% select(smoothed_valence) %>% unlist()
  prediction_valence_b <- data %>% filter(subject == subject_b) %>% select(smoothed_valence) %>% unlist()

  trajectory_valence <- coef(lm(prediction_valence_a-prediction_valence_b ~ start))[2]

  prediction_arousal_a <- data %>% filter(subject == subject_a) %>% select(smoothed_arousal) %>% unlist()
  prediction_arousal_b <- data %>% filter(subject == subject_b) %>% select(smoothed_arousal) %>% unlist()

  trajectory_arousal <- coef(lm(prediction_arousal_a-prediction_arousal_b ~ start))[2]


  prediction_dominance_a <- data %>% filter(subject == subject_a) %>% select(smoothed_dominance) %>% unlist()
  prediction_dominance_b <- data %>% filter(subject == subject_b) %>% select(smoothed_dominance) %>% unlist()

  trajectory_dominance <- coef(lm(abs(prediction_dominance_a-prediction_dominance_b) ~ start))[2]

  return(tibble(valence_alignment = trajectory_valence,
                arousal_alignment = trajectory_arousal,
                dominance_alignment = trajectory_dominance))
}


ztc_trajectory_scores <- ztc_smoothed_annotations %>%
  group_by(session, task) %>%
  group_modify(~trajectory_per_subject(.))

alignedness <- function(data) {
  subjects <- data %>% select(subject) %>% distinct() %>% unlist()
  subject_a <- subjects[1]
  subject_b <- subjects[2]
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