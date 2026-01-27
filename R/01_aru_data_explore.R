#############################
## 01_aru_data_explore.R
## input: Wildtrax aru tag downloads,
##        and station location data (aggregated in all_sensors/02_mergs_station_locations.R)
## output: 
##
## July 10 2025
#############################


#### Environment set up ####
# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("activity",
                      "corrplot",
                      "cowplot",
                      "data.table",
                      "dplyr", 
                      "elevatr",
                      "gfcanalysis",  
                      "ggplot2",
                      "gridExtra",
                      "iNEXT",
                      "kableExtra",
                      "Hmsc",
                      "leaflet",
                      "lme4",
                      "lubridate",
                      "magrittr",
                      "MCMCvis",
                      "modisfast",
                      "osmdata",
                      "pals",
                      "plotly",
                      "purrr",
                      "remotes",
                      "rmarkdown",
                      "sf",
                      "spOccupancy",
                      "stars",
                      "stringr",
                      "terra",
                      "tibble",
                      "tidyr", 
                      "unmarked",
                      "viridis",
                      "jtools",
                      "vegan",
                      "MuMIn",
                      "usedist",
                      "taxize")

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


#### Load ARU station locations ####
### NOTE (12 Jan 2026: need to update data loading - locations and tags - using direct download with wildrtrax)

aru_stns <- read.csv("data/NWTBM_aru_locations_July162025.csv")

## Create subset df of aru location, study area, lat-long
aru_coords <- aru_stns %>%
  select(location, study_area, latitude, longitude)


# #### Load ARU tag data from WildTrax - not working in Jan 26 2026 - Eamon mentioned there was an issue on the WT end ####
# library(wildrtrax)
# 
# ## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
# source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
# wt_auth()
# 
# ## Get data from relevant WildTrax projects
# aru_projects <- wt_get_projects("ARU") %>% 
#   filter(organization_name == "CWS-NOR") # filter for CWS North projects
# glimpse(aru_projects)
# 
# ## There are 4 Edehzhie projects (project_id): 2016 (46), June 2019 (172), Total 2019 (2478), and 2021 (1052) -- don't use Total 2019, it doesn't have manually-tagged observations
# ## There are 3 Gameti projects (project_id): June 2023 (3212), June 2024 (3213), and June - Sep 2024 (3007) -- all part of the same overall project, no overlapping recordings (just overlapping June periods)
# ## Other projects (project_id): 1254 (Thaidene Nene), 2333 (Fort Smith), 2373 (Norman Wells), 1696 (Sambaa K'e)
# 
# ## Filter to my target projects only, using project IDs:
# aru_projects <- aru_projects %>% filter(project_id == "46" |
#                                           project_id == "172" |
#                                           project_id == "1052" |
#                                           project_id == "3212" |
#                                           project_id == "3213" |
#                                           project_id == "3007" |
#                                           project_id == "1254" |
#                                           project_id == "2333" |
#                                           project_id == "2373" |
#                                           project_id == "1696")
# 
# 
# ## Get raw ARU data from all projects
# aru_data <- wt_download_report(project_id = aru_projects$project_id,
#                                sensor_id = "ARU",
#                                report = "main") # main reports include ALL DATA





#### Manual data download (prior to authentication into WildTrax) #####
## This is a long-form report of all species tags created by the expert observer.
## Confirm NWTBM recording tagging protocol to understand how species were tagged
## Change working directory to aru tag data folder
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/wildtrax_download_aru/MainReports") ## main reports include ALL DATA


list.files() 
## Includes all 3 Gameti projects (2023, 2024, and 2023-2024 with longer sampling period) and 3 of the Edehzhie projects (June 2016, 2019, and 2021. Didn't include untagged  2019 data)
## Note that to compare with camera projects, I should  only be using 2021 Edehzhie data to match temporal periods of camera deployments



## Object listing all csvs
aru_main_list <- list.files(pattern = "\\.csv$")
aru_main_list

## Remove unneeded Edehzhie files (2016 and 2019)
aru_main_list <- aru_main_list[!str_detect(aru_main_list, "2016")]
aru_main_list <- aru_main_list[!str_detect(aru_main_list, "2019")]

# Read and bind all CSVs, adding a column for the source file
aru_data <- rbindlist(lapply(aru_main_list, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds") ## set back to main project directory

glimpse(aru_data) ## 71 826 rows, 32 variables
## Name the different source files
table(aru_data$source_file)

## Add a study area column based on the source file name
aru_data <- aru_data %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    str_detect(source_file, "TDN") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))
## Check the study area column
table(aru_data$study_area)

## Are any study areas NA?
sum(is.na(aru_data$study_area)) ## 0 NAs - so all source files have been matched to a study area

## Remove source_file column
aru_data <- aru_data %>%
  select(-source_file)

glimpse(aru_data)

## How many locations have species data?
length(unique(aru_data$location)) ## 742 locations have data

## How many species were tagged?
length(unique(aru_data$species_common_name)) #199

## Does aru_data include recordings with no species info (i.e., no tags)?
sum(is.na(aru_data$species_common_name)) ## none - all recordings have at least one species tag


### Create a df that summarises number of tags by species within each study area

species_tag_sum <- aru_data %>%
  group_by(study_area, species_common_name) %>%
  summarise(tag_count = n(), .groups = "drop") %>%
  arrange(study_area, desc(tag_count))

## How many tags are there for my target species?
target_spp <- c("Rock Ptarmigan", "Ruffed Grouse",
                "Sharp-tailed Grouse", "Spruce Grouse",
                "Willow Ptarmigan")


target_sum <- species_tag_sum %>%
  filter(species_common_name %in% target_spp)

## Filter aru_data for target species
target_aru <- aru_data %>%
  filter(species_common_name %in% target_spp) ## 905 tags for target species


### Faceted plot of target species tags for each study area
win.graph()
f_bar_bird <- ggplot(target_sum, aes(x = reorder(study_area, -tag_count), y = tag_count, fill = species_common_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ species_common_name, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Recording Tags by Study Area",
    x = "Study Area",
    y = "Total Tags"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none" # remove legend
  )

f_bar_bird

## Save bar plot
ggsave("figures/ARU_tags_by_SA_20250711.jpeg", plot = f_bar_bird, width = 10, height = 6)

#### Summarize tag data by recording ####
glimpse(target_aru)

## Sum total unique recording_id
length(unique(target_aru$recording_id)) ## 624 unique recordings vs 905 unique tags - so multiple recordings have +1 tags for target species

### Convert target_aru to detection/non-detection format by recording_id, where if a recording has at least one target species tag, it is a 1 (detection), otherwise 0 (non-detection)
gb_aru_detect <- target_aru %>%
  group_by(recording_id, species_common_name, species_code, location, study_area, recording_date_time) %>%
  summarise(detection = ifelse(n() > 0, 1, 0), .groups = "drop")

glimpse(gb_aru_detect) ## 642 rows - so some recordings have multiple target species detected

## Total detections by study area and species
gb_aru_detect_sum <- gb_aru_detect %>%
  group_by(study_area, species_common_name) %>%
  summarise(detect_count = sum(detection), .groups = "drop") %>%
  arrange(study_area, desc(detect_count))

## Replicate bar plot for detection/non-detection per recording

win.graph()
f_bar_gb_det <- ggplot(gb_aru_detect_sum, aes(x = reorder(study_area, -detect_count), y = detect_count, fill = species_common_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ species_common_name, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Game Bird Detections by Study Area",
    x = "Study Area",
    y = "Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none" # remove legend
  )

f_bar_gb_det
## Save bar plot
ggsave("figures/ARU_detections_by_SA_20260126.jpeg", plot = f_bar_gb_det, width = 10, height = 6)


#### Exploratory phenology ####
glimpse(gb_aru_detect) ## number of rows here should = sum across all species in target_sum

sum(gb_aru_detect_sum$detect_count) # 642 - confirmed

## Generate 1 histogram for the dates of detections for each species. 
## Start with one species, one study area to simplify - WIPT, TDN

## First need to isolate date in recording_date_time
class(gb_aru_detect$recording_date_time)

gb_aru_detect$recording_date <- as.Date.POSIXct(gb_aru_detect$recording_date_time, tz = "UTC")
class(gb_aru_detect$recording_date)
## Many datasets include data across multiple years
## Create a column for month-day
gb_aru_detect$recording_month_day <- format(gb_aru_detect$recording_date, "%m-%d")

## Converting month_day column to factor with levels organized by calendar days
gb_aru_detect$recording_month_day <- factor(gb_aru_detect$recording_month_day,
                                 levels = format(seq(as.Date("2000-01-01"),
                                                     as.Date("2000-12-31"),
                                                     by = "day"), "%m-%d"))
class(gb_aru_detect$recording_month_day)

# Get every other day for x-axis breaks
every_other_day <- levels(gb_aru_detect$recording_month_day)[seq(1, length(levels(gb_aru_detect$recording_month_day)), by = 2)]

## Histogram of detections across all study areas, sorted by species
win.graph()
call_dates <- ggplot(gb_aru_detect, 
                     aes(x = recording_month_day)) + ## specifying month-day in factor format
  geom_histogram(stat = "count", fill = "steelblue", color = "black") + 
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  scale_x_discrete(breaks = every_other_day) + ## show every other day on x-axis
  labs(
    title = "Detection Phenology",
    x = "Date",
    y = "Total Detections"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) ## angling x-axis text to help with readability



call_dates


#### Tagged recording's aren't just in June!! For all data (aru_data), what months have tagged data? ####
## Create a column for month-day in aru_data
aru_data$recording_date <- as.Date.POSIXct(aru_data$recording_date_time, tz = "UTC")
aru_data$recording_month_day <- format(aru_data$recording_date, "%m-%d")
table(is.na(aru_data$recording_month_day))

## Converting month_day column to factor with levels organized by calendar days
aru_data$recording_month_day <- factor(aru_data$recording_month_day,
                                         levels = format(seq(as.Date("2000-01-01"),
                                                             as.Date("2000-12-31"),
                                                             by = "day"), "%m-%d"))
# Get every 5th day for x-axis breaks
every_fifth_day <- levels(target_aru$recording_month_day)[seq(1, length(levels(target_aru$recording_month_day)), by = 5)]


## Scatter plot of total detections (all species) by month-day
tagged_months <- ggplot(aru_data, aes(x = recording_month_day, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0)) +
  scale_x_discrete(breaks = every_fifth_day) +
  labs(
    title = "Phenology of tagged data in WildTrax",
    x = "Month-Day",
    y = "Total tags",
    color = "Study Area"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tagged_months

## Scatter plot of total detections (all species) by recording_date (to see which study areas have multiple years)
tagged_years <- ggplot(aru_data, aes(x = recording_date, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0)) +
  labs(
    title = "Phenology of tagged data in WildTrax",
    x = "Deployment Year",
    y = "Total tags",
    color = "Study Area"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tagged_years 

##### Scatter plot of target species by recording_date (repeat of first histogram, but as a scatterplot and color coding by study area) ####
gb_tagged_years <- ggplot(gb_aru_detect, aes(x = recording_date, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  labs(
    title = "Total game bird detections in WildTrax recordings",
    x = "Deployment Year",
    y = "Total detections",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_years 

## Save scatterplots of total tagged gamebird data in WildTrax (by year)
ggsave("figures/gamebird_detections_depyears_20260126.jpeg", plot = gb_tagged_years, width = 10, height = 6)

##Combining all gamebirds
gb_tags_combined <- ggplot(target_aru, aes(x = recording_date, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  labs(
    title = "Total tagged game bird data in WildTrax",
    x = "Deployment Year",
    y = "Total tags",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tags_combined
## Saving 
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/gamebirds_combined_tag_depyears_Dec022025.jpeg", plot = gb_tags_combined, width = 10, height = 6)

## Scatter plot of target spp detections by month-day, faceted by species code (seasonal patterns)
gb_tagged_months <- ggplot(gb_aru_detect, aes(x = recording_month_day, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_discrete(breaks = every_fifth_day) +
  facet_wrap(~ species_code, scales = "free_y") +
  labs(
    title = "Phenology of game bird detections in WildTrax",
    x = "Month-Day",
    y = "Total Detections",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_months

## Convert back to histogram to compare
gb_phen <- ggplot(gb_aru_detect, 
                     aes(x = recording_month_day, y = after_stat(count), fill = study_area)) + ## specifying month-day in factor format
  geom_histogram(stat = "count") + 
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  scale_x_discrete(breaks = every_other_day) + ## show every other day on x-axis
  labs(
    title = "Phenology of game bird detections in WildTrax",
    x = "Month-Day",
    y = "Total Detections"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) ## angling x-axis text to help with readability

gb_phen

## Scatter plot is easiest to read, save that one
ggsave("figures/gamebird_detection_phenology_20260126.jpeg", plot = gb_tagged_months, width = 10, height = 6)




#### Activity period of vocalizations throughout day
## Add a column for time of day (hour-minute-second) from recording_date_time
gb_aru_detect$recording_time <- format(gb_aru_detect$recording_date_time, "%H:%M:%S")
class(gb_aru_detect$recording_time) # character
## Convert to POSIXct time format
gb_aru_detect$recording_time <- as.POSIXct(gb_aru_detect$recording_time, format = "%H:%M:%S", tz = "UTC")
class(gb_aru_detect$recording_time) # POSIXct

length(unique(gb_aru_detect$recording_time)) ##238 - so no standard recording intervals across ARUs

## Scatter plot of target spp detections by time of day, faceted by species code (daily activity patterns)
win.graph()
gb_tagged_times <- ggplot(gb_aru_detect, aes(x = recording_time, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") +
  facet_wrap(~ species_code, scales = "free_y") +
  labs(
    title = "Activity period of game bird detections in WildTrax",
    x = "Time",
    y = "Total Detections",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_times 

## Save plot
ggsave("figures/gamebird_detection_activityperiod_20260126.jpeg", plot = gb_tagged_times, width = 10, height = 6)

## Is this representative of actual activity patterns, or just the random sample of recording times?

## Need a histogram of all recording times (regardless of target species tags) to compare
glimpse(aru_data)

## Create a df of unique recording_id and recording_time
rec_df <- aru_data %>%
  select(recording_id, recording_date_time) %>%
  distinct()
## Add recording_time column
rec_df$recording_time <- format(rec_df$recording_date_time, "%H:%M:%S")
# Convert to POSIXct time format
rec_df$recording_time <- as.POSIXct(rec_df$recording_time, format = "%H:%M:%S", tz = "UTC")

## Scatter plot of all recording times
win.graph()
rec_times <- ggplot(rec_df, aes(x = recording_time, y = after_stat(count))) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "4 hours") +
  labs(
    title = "Times for Recordings Sampled in WildTrax",
    x = "Time",
    y = "Count of Recordings") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rec_times ## right, so game bird detections follow the sampled recording times - NOT representative of actual activity patterns

## Save plot
ggsave("figures/aru_wt_sampled_recording_times_20260127.jpeg", plot = rec_times, width = 10, height = 6)
