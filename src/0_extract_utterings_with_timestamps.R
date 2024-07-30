source("config.R") # load config variables

library(emuR)

ztc_data <- load_emuDB(raw_data_path)

# Extract utterings with timestamps
ztc_utterings <- query(ztc_data,
                       query = "ORT =~ .*",
                       timeRefSegmentLevel = "MAU")

# Save the extracted utterings
saveRDS(ztc_utterings, file = "data/ztc_utterings.rds")