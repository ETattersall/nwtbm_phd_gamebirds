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

aru_stns <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_aru/NWTBM_aru_locations_July162025.csv")

## Create subset df of aru location, study area, lat-long
aru_coords <- aru_stns %>%
  select(location, study_area, latitude, longitude)


#### Load ARU tag data from WildTrax ####
## This is a long-form report of all species tags created by the expert observer.
## Confirm NWTBM recording tagging protocol to understand how species were tagged
## Reset working directory
getwd()
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/wildtrax_download_aru/SpeciesTags")
list.files()
## Object listing all csvs
aru_tag_csv <- list.files(pattern = "\\.csv$")


# Read and bind all CSVs, adding a column for the source file
aru_data <- rbindlist(lapply(aru_tag_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

glimpse(aru_data) ## 71 815 rows
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
length(unique(aru_data$location)) ## 742 locations have data - same as unique locations in aru_coords

## How many species were tagged?
length(unique(aru_data$species_common_name)) #198


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
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/ARU_tags_by_SA_20250711.jpeg", plot = f_bar_bird, width = 10, height = 6)

#### Exploratory phenology ####
glimpse(target_aru) ## number of rows here should = sum across all species in target_sum

sum(target_sum$tag_count) # 905 - confirmed

## Generate 1 histogram for the dates of detections for each species. 
## Start with one species, one study area to simplify - WIPT, TDN

## First need to isolate date in recording_date_time
class(target_aru$recording_date_time)

target_aru$recording_date <- as.Date.POSIXct(target_aru$recording_date_time, tz = "UTC")
class(target_aru$recording_date)
## Many datasets include data across multiple years
## Create a column for month-day
target_aru$recording_month_day <- format(target_aru$recording_date, "%m-%d")

## Converting month_day column to factor with levels organized by calendar days
target_aru$recording_month_day <- factor(target_aru$recording_month_day,
                                 levels = format(seq(as.Date("2000-01-01"),
                                                     as.Date("2000-12-31"),
                                                     by = "day"), "%m-%d"))
class(target_aru$recording_month_day)

# Get every other day for x-axis breaks
every_other_day <- levels(target_aru$recording_month_day)[seq(1, length(levels(target_aru$recording_month_day)), by = 2)]

## Histogram of detections across all study areas, sorted by species
win.graph()
call_dates <- ggplot(target_aru, 
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


## Tagged recording's aren't just in June!! For all data (aru_data), what months have tagged data?
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

## Scatter plot of target species by recording_date (repeat of first histogram, but as a scatterplot and color coding by study area)
gb_tagged_years <- ggplot(target_aru, aes(x = recording_date, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  labs(
    title = "Total tagged game bird data in WildTrax",
    x = "Deployment Year",
    y = "Total tags",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_years 

## Save scatterplots of total tagged gamebird data in WildTrax (by year)
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/gamebird_tag_depyears_Dec022025.jpeg", plot = gb_tagged_years, width = 10, height = 6)

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
gb_tagged_months <- ggplot(target_aru, aes(x = recording_month_day, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_discrete(breaks = every_fifth_day) +
  facet_wrap(~ species_code, scales = "free_y") +
  labs(
    title = "Phenology of tagged game bird data in WildTrax",
    x = "Month-Day",
    y = "Total tags",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_months

## Convert back to histogram to compare
gb_phen <- ggplot(target_aru, 
                     aes(x = recording_month_day, y = after_stat(count), fill = study_area)) + ## specifying month-day in factor format
  geom_histogram(stat = "count") + 
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  scale_x_discrete(breaks = every_other_day) + ## show every other day on x-axis
  labs(
    title = "Phenology of tagged game bird data in WildTrax",
    x = "Month-Day",
    y = "Total Tags"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) ## angling x-axis text to help with readability

gb_phen

## Scatter plot is easiest to read, save that one
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/gamebird_tag_phenology_Dec022025.jpeg", plot = gb_tagged_months, width = 10, height = 6)


#### Load Recordings Report (just to check if all locations have recordings) ####
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/wildtrax_download_aru/RecordingReports")
list.files()
## Object listing all csvs
recording_csv <- list.files(pattern = "\\.csv$") ## Two TDN projects (one called TDN, other called Thaidene Nene), 3 Gameti projects - 9 files total


# Read and bind all CSVs, adding a column for the source file
recording_data <- rbindlist(lapply(recording_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

glimpse(recording_data) ## 7050 - fewer recordings than species tags. I guess that makes sense (multiple species tags per recording)

## Need to generate study area column based on project names
table(recording_data$source_file)

recording_data <- recording_data %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "SambaaK'e") ~ "SambaaK'e",
    str_detect(source_file, "TDN") ~ "ThaideneNëné",
    str_detect(source_file, "Thaidene Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

length(unique(recording_data$study_area))

## Remove source_file column
recording_data <- recording_data %>%
  select(-source_file)



### ARU presence-absence matrix - this will only include stations that have species tags in WildTrax
