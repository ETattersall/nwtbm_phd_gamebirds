###########################
## 04_HawkEars_data_cleaning.R
## Erin Tattersall
## May 14 2026
###########################

# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("tidyverse",
                      "purrr",
                      "stringr",
                      "here", # helps find project files (and set root directories)
                      "withr", # to temporarily change directories
) 

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Quick check of HawkEars output ####
list.files("/home/tatterer/he_output/batch1")

scores <- read.csv("/home/tatterer/he_output/batch1/scores.csv")

glimpse(scores)
summary(scores)
table(scores$name)

## Filter for grouse and ptarmigan spp (though based on species verification results, I will be focusing mainly on RUGR, WIPT, possibly STGR)
tar_spp <- c("ROPT", "RUGR", "SPGR", "STGR", "WIPT")

gb_scores <- scores %>% 
  filter(name %in% tar_spp) ## 4187 observations

## Add a column for location, date, and time based on the parts of the recording name
gb_scores <- gb_scores %>%
  mutate(
    recording_parts = strsplit(recording, "_")
  ) %>%
  mutate(
    location = sapply(recording_parts, `[`, 1),
    date     = as.Date(sapply(recording_parts, `[`, 2), format = "%Y%m%d"),
    time     = format(strptime(sapply(recording_parts, `[`, 3), "%H%M%S"), "%H:%M:%S")
  ) %>%
  select(-recording_parts) %>% 
  arrange(recording, location, date, time, start_time, end_time, score)


glimpse(gb_scores)
summary(gb_scores)

table(gb_scores$name) ## 1436 RUGR, 77 STGR - note these are multiple detections per recording

## Summarize scores by species
spp_scores_sum <- gb_scores %>% group_by(name) %>% summarise(
  min_score = min(score),
  mean_score = mean(score),
  max_score = max(score)
)

spp_scores_sum

### Save results for model pre-processing (not yet run for this test)
write.csv(gb_scores, "data/HawkEars_gamebirddetections_ENWA-O-01-01_202205.csv")


### Cleaning steps still needed: filtering by pre-determined confidence threshold (still need to calculate exact threshold for RUGR, WIPT, and STGR)