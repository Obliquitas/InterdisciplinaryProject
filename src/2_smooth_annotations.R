library(tidyr)
library(dplyr)
library(mgcv)

# Load annotated utterings
ztc_annotated_utterings <- readRDS("data/ztc_annotated_utterings.rds")
# Smooth the annotated values by using a gam model
ztc_smoothed_annotations <- ztc_annotated_utterings %>%
  group_by(bundle) %>%
  group_modify(~{
    if (length(.$valence) > 5) {
      gam_model <- gam(valence ~ s(start, k = 5), data = .)
      smoothed_values <- predict(gam_model, newdata = .)
      bind_cols(., tibble(smoothed_valence = as.numeric(smoothed_values)))
    } else {
      bind_cols(., tibble(smoothed_valence = .$valence))
    }
  }) %>%
  group_modify(~{
    if (length(.$arousal) > 5) {
      gam_model <- gam(arousal ~ s(start, k = 5), data = .)
      smoothed_values <- predict(gam_model, newdata = .)
      bind_cols(., tibble(smoothed_arousal = as.numeric(smoothed_values)))
    } else {
      bind_cols(., tibble(smoothed_arousal = .$arousal))
    }
  }) %>%
  group_modify(~{
    if (length(.$dominance) > 5) {
      gam_model <- gam(dominance ~ s(start, k = 5), data = .)
      smoothed_values <- predict(gam_model, newdata = .)
      bind_cols(., tibble(smoothed_dominance = as.numeric(smoothed_values)))
    } else {
      bind_cols(., tibble(smoothed_dominance = .$dominance))
    }
  })

# ungroup
ztc_smoothed_annotations <- ztc_smoothed_annotations %>% ungroup()

# Save the smoothed annotations
saveRDS(ztc_smoothed_annotations, file = "data/ztc_smoothed_annotations.rds")
