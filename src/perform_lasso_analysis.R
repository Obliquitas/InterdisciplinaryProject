library(tidyr)
library(dplyr)
library(glmnet)
library(plotmo)

#Print wd
getwd()

source("src/smooth_annotations.R")
source("src/calculate_emotion_metrics.R")
source("src/get_lasso_model.R")
source("src/gam_analysis.R")

ztc_annotated_utterings <- readRDS("data/ztc_annotated_utterings.rds")
ztc_annotated_utterings_ser <- readRDS("data/ztc_annotated_utterings_ser.rds")

annotations_emotion_metrics <- ztc_annotated_utterings %>%
  smooth_annotations %>%
  calculate_emotion_metrics

annotations_data <-  get_model_dataframe(annotations_emotion_metrics,
                                         ztc_annotated_utterings)

ser_emotion_metrics <- ztc_annotated_utterings_ser %>%
  smooth_annotations %>%
  calculate_emotion_metrics


ser_data <- get_model_dataframe(ser_emotion_metrics,
                                ztc_annotated_utterings_ser)

sink("output/lasso_analysis.txt")

lasso_analysis(annotations_data, "annotations")
lasso_analysis(ser_data, "ser")

sink(NULL)

gam_analyis(annotations_data, "annotations")
gam_analyis(ser_data, "ser")