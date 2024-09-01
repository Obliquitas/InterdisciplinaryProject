# Read annotations from CSV file and convert to tibble

library(tidyr)
library(dplyr)

# Load annotations
annotations <- read.csv("data/annotations.csv")

# Convert to tibble
annotations_tibble <- as_tibble(annotations)

# Save the tibble
saveRDS(annotations_tibble, file = "data/ztc_annotated_utterings_ser.rds")