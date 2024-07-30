library(stopwords)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)

ztc_utterings <- readRDS("data/ztc_utterings.rds")
stopwords <- stopwords("de", source = "stopwords-iso")

# Remove stopwords
ztc_relevant_utterings <- ztc_utterings %>%
  filter(! labels %in% stopwords)

# Split columns with multiple variables into separate columns
ztc_relevant_utterings <- ztc_relevant_utterings %>%
  mutate(recording = substring(session, 5),
         task = substring(bundle, 6, 6),
         pairing = substring(session, 1, 4),
         subject = substring(bundle, 8, 9))


# Create lookup dictionary for emotion scores we get
# the dominance scores from memolon and the rest from Köper et Schulte im Walde

memolon <- read_tsv("data/memolon/de.tsv") %>%
  select(word, dominance)

de_emo <- fread("data/de_emo_norms/de_emo_norms.txt") %>%
  select(word = WORD, valence = Valency, arousal = Arousal)

lookup_dict <- merge(memolon, de_emo, by.x = "word", by.y = "word")

# Annotate the utterings using the lookup dictionary
ztc_annotated_utterings <- merge(ztc_relevant_utterings,
                                 lookup_dict,
                                 by.x = "labels",
                                 by.y = "word")

# Arrange by start time
ztc_annotated_utterings <- ztc_annotated_utterings %>%
  arrange(start)


# Save the annotated utterings
saveRDS(ztc_annotated_utterings, file = "data/ztc_annotated_utterings.rds")