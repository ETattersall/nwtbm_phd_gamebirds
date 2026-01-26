### 02_cam_data_explore.R ####
## Downloading camera data for WildTrax and preliminary exploration of gamebird detections
## Started by Erin Tattersall on 12 January 2026
################################################

library(tidyverse)

## Needed to download station locations
## remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
library(wildrtrax)

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()



#### Station locations from wildtrax ####
cam_projects <- wt_get_projects("CAM")
glimpse(cam_projects) ## lists all the projects I have access to - including public projects I'm not involved in

## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
cam_projects <- cam_projects %>% filter(project_id == "712" |
                                        project_id == "2183" |
                                        project_id == "2102" |
                                        project_id == "1906" |
                                        project_id == "2935" |
                                        project_id == "1465")

## Get raw camera data from all projects (note that this is PRE- species verification for game birds (12 Jan 2026), so will need to be re-downloaded once that's complete)
cam_data <- wt_download_report(project_id = cam_projects$project_id,
                               sensor_id = "CAM",
                               report = "main") # main reports include ALL DATA

## isolate tag data from list of data frames
glimpse(cam_data) ## list of 6 data frames, one per project (it's a list so it can be indexed)

## Rename each data frame in cam_data list to simpler study area names
names(cam_data) <- c("Edéhzhíe", "SambaaK'e", "FortSmith", "Gamètì", "NormanWells", "ThaideneNëné")
names(cam_data)


## Save cam_data list as RDS file for future use
saveRDS(cam_data, file = "data/cam_data_all_projects_20260112.rds")
## Note that for most projects, station coordinate data is not stored on WildTrax
## Station location data will need to be uploaded separately


## For each data frame in cam_data list, get individual species detections within a 30-minute independence threshold
spp_det <- lapply(cam_data, 
                  wt_ind_detect, 
                  threshold = 30, 
                  units = "minutes")

# How many species are named in total?
spp_count <- lapply(spp_det, function(df) {
  length(unique(df$species_common_name))
})
spp_count ## returns a list of total species per study area

##### filter to target game bird species only ####
target_species <- c("Ruffed Grouse", "Spruce Grouse", "Sharp-tailed Grouse", "Willow Ptarmigan", "Rock Ptarmigan")
spp_det_gb <- lapply(spp_det, function(df) {
  df %>% filter(species_common_name %in% target_species)
})

spp_det_gb_count <- lapply(spp_det_gb, function(df) {
  length(unique(df$species_common_name))
}) ## returns a list of total game bird species per study area
spp_det_gb_count

## Bind all gamebird dfs in spp_det_gb list into a single data frame with study area as an identifier column
gb_df <- bind_rows(spp_det_gb, .id = "study_area")
glimpse(gb_df)

### Save gamebird independent detection data frame
write.csv(gb_df, "data/all_projects_gamebirds_cam_ind_detections_20260112.csv", row.names = FALSE)

## Summarise the gb data by study area and detection count (ordered by study area and species)
gb_count <- gb_df %>%
  group_by(study_area, species_common_name) %>%
  summarise(count = n()) %>%
  mutate(study_area = fct_reorder(study_area, count, .desc = TRUE)) %>% 
  ungroup()

gb_count

## Faceted bar plot figure of species detections by study area
gb_plot1 <- ggplot(gb_count, aes(x = reorder(study_area, -count), y = count, fill = species_common_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ species_common_name, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Game Bird Detections by Study Area",
    x = "Study Area",
    y = "Independent Detections (30 min.)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none" # remove legend
  )

win.graph()
gb_plot1


## Save plot
ggsave("figures/camera_ind_detections_gamebirds_20260112.jpeg", plot = gb_plot1, width = 10, height = 6, units = "in", dpi = 300)


#### Exploratory phenology ####
glimpse(gb_df) #df has both start_time and end_time of independent detections
class(gb_df$start_time) #POSIXct

## Add species codes column to match ARU data
table(gb_df$species_common_name)
gb_df <- gb_df %>%
  mutate(species_code = case_when(
    species_common_name == "Ruffed Grouse" ~ "RUGR",
    species_common_name == "Spruce Grouse" ~ "SPGR",
    species_common_name == "Sharp-tailed Grouse" ~ "STGR",
    species_common_name == "Willow Ptarmigan" ~ "WIPT",
    species_common_name == "Rock Ptarmigan" ~ "ROPT"))

## Histogram of detection dates by study area
gb_plot2 <- ggplot(gb_df, aes(x = as.Date(start_time), fill = species_common_name)) +
  geom_histogram(binwidth = 7, position = "dodge") +
  facet_wrap(~ study_area, ncol = 1) +
  theme_minimal() +
  labs(
    title = "Game Bird Detection Dates by Study Area",
    x = "Detection Date",
    y = "Number of Independent Detections (30 min.)"
)

gb_plot2 ## detections occur in diff years for different study areas (Expected)

## Save plot
ggsave("figures/camera_gamebird_detection_years_20260112.jpeg", plot = gb_plot2, width = 10, height = 8, units = "in", dpi = 300)


### Histogram by month-day (ignoring year) to see seasonal patterns across all years
## Create column for month-day
gb_df$det_month_day <- format(gb_df$start_time, "%m-%d") 

# Convert month-day column to factor with levels ordered by calendar days
gb_df$det_month_day <- factor(gb_df$det_month_day,
                                         levels = format(seq(as.Date("2000-01-01"),
                                                             as.Date("2000-12-31"),
                                                             by = "day"), "%m-%d"))
class(gb_df$det_month_day) #factor


# Get every other day for x-axis breaks
every_other_day <- levels(gb_df$det_month_day)[seq(1, length(levels(gb_df$det_month_day)), by = 2)]

## Histogram of detections across all study areas, sorted by species
win.graph()
call_dates <- ggplot(gb_df, 
                     aes(x = det_month_day)) + ## specifying month-day in factor format
  geom_histogram(stat = "count", fill = "steelblue", color = "black") + 
  facet_wrap(~ species_code, scales = "free_y") + ## faceting by species code
  scale_x_discrete(breaks = every_other_day) + ## show every other day on x-axis
  labs(
    title = "Detection Phenology",
    x = "Date",
    y = "Total Independent Detections"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) ## angling x-axis text to help with readability

call_dates

## Try scatter plot instead, marking every 30 days on x-axis

# Get 30 days for x-axis breaks
every_30_days <- levels(gb_df$det_month_day)[seq(1, length(levels(gb_df$det_month_day)), by = 30)]
## Scatter plot of target spp detections by month-day, faceted by species common name (seasonal patterns)
gb_tagged_months <- ggplot(gb_df, aes(x = det_month_day, y = after_stat(count), color = study_area)) +
  geom_point(stat = "count", position = position_jitter(width = 0.3, height = 0), size = 2) +
  scale_x_discrete(breaks = every_30_days) +
  facet_wrap(~ species_code, scales = "free_y") +
  scale_y_continuous(
    breaks = function(x) {
      rng <- range(x, na.rm = TRUE)
      seq(floor(rng[1]), ceiling(rng[2]), by = 1)  # integers only
    },
    minor_breaks = NULL) + # optional: declutter minor grid lines
  labs(
    title = "Phenology of Game Bird Camera Data",
    x = "Month-Day",
    y = "Independent Detections",
    color = "Study Area"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gb_tagged_months
## Save plot
ggsave("figures/gamebird_camera_detection_phenology_20260112.jpeg", plot = gb_tagged_months, width = 10, height = 6, units = "in", dpi = 300)
### End of 02_cam_data_explore.R ####