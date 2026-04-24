#############################################
## HawkEars_species_verification_CWSgamebirds.R
## Testing HawkEars and verifying species IDs on more of CWS-NOR data in WT
## Started on Apr 23 2026
## Created by Erin Tattersall
#############################################


#### Environment set-up ####

# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("wildrtrax",
                      "tidyverse",
                      "kableExtra",
                      "webshot2",
                      "chromote",
                      "ggplot2",
                      "cowplot",
                      "RColorBrewer",
                      "here", # helps find project files (and set root directories)
                      "withr", # to temporarily change directories
                      "reticulate") #enables coding in python

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

### Load ARU tag data from WildTrax ####
## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD (wildtrax_login.R - not shared on GitHub)
## Note - may need to authenticate into WT a few times in a script if it times out
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD (adding here() ensures source searches the root directory)
wt_auth()

## Get data from relevant WildTrax projects
aru_projects <- wt_get_projects("ARU") %>%
  filter(organization_name == "CWS-NOR" &  # filter for CWS North projects
    project_status == "Active") # filter for my Active projects only


str(aru_projects)

## Testing whether public reports (and media) can be downloaded
aru_pub <- wt_get_projects("ARU") %>%
  filter(organization_name == "CWS-NOR" &  # filter for CWS North projects
        project_status == "Published - Public" # filter for Public data only
           )
## Also look at public CWS-Prairie projects
pra_proj <- wt_get_projects("ARU") %>%
  filter(organization_name == "CWS-PRA" &  # filter for CWS North projects
           project_status == "Published - Public" # filter for Public data only
  )

## Get tag reports from all projects - that is, all species tags recorded for each project
pub_tags <- wt_download_report(
  project_id = aru_pub$project_id,
  sensor_id  = "ARU",
  report     = "tag")

## Try downloading reports one project at a time and skipping errors (since these projects keep failing to download)
aru_tags <- lapply(aru_projects$project_id, function(pid) {
  tryCatch(
    wt_download_report(
      project_id = pid,
      sensor_id  = "ARU",
      report     = "tag"
    ),
    error = function(e) {
      message("Skipping project ", pid, ": ", e$message) # if an error occurs while downloading a project, display message that project has been skipped (and display error message)
      NULL
    }
  )
})

## Download public prairie projects
pra_tags <- lapply(pra_proj$project_id, function(pid) {
  tryCatch(
    wt_download_report(
      project_id = pid,
      sensor_id  = "ARU",
      report     = "tag"
    ),
    error = function(e) {
      message("Skipping project ", pid, ": ", e$message)
      NULL
    }
  )
})


## Create a single df of all pub_tags, aru_tags, and pra_tags
tags_nor <- bind_rows(aru_tags)
pub_tags <- bind_rows(pub_tags)
pra_tags <- bind_rows(pra_tags)
all_tags <- bind_rows(tags_nor, pub_tags, pra_tags)

## Create a list of target gamebird species codes
tar_spp <- c("ROPT", "RUGR", "SPGR", "STGR", "WIPT")

## Isolate gamebird tags
cws_gb <- all_tags %>% 
  filter(species_code %in% tar_spp)

## Remove any rows that don't have clip or spectrogram URLs
cws_gb <- cws_gb %>%
  filter(if_all(c(spectrogram_url, clip_url), ~ !is.na(.)))


table(cws_gb$species_code) 
## ROPT RUGR SPGR STGR WIPT 
##  77 1329  211  324 1221

## Enough of each species except ROPT, but that's understandable. 

## Download target species audio files
cws_tar_rec <- wt_download_media(
      input = cws_gb,
      output = "data/wildtrax_download_aru/audio/CWS_gamebirds",
      type = "tag_clip_audio")


## Testing tryCatch to finish running even if errors are encountered 
## applying each row of cws_gb as if they were single clips in a list (full data from one tag downloaded at a time)
## and including an error message for clips that fail
cws_tar_rec <- lapply(seq_len(nrow(cws_gb)), function(i) {
  tryCatch(
    {
      wt_download_media(
        input  = cws_gb[i, ],
        output = "data/wildtrax_download_aru/audio/CWS_gamebirds",
        type   = "tag_clip_audio"
      )
      TRUE
    },
    error = function(e) {
      message("Skipping clip ", i, ": ", e$message)
      FALSE
    }
  )
})
## Skipped clips 2216 - 2836: all SKBMS locations
## remove these from the df
cws_gb2 <- cws_gb[-c(2216:2836), ]

## now check species clips (i.e., how many of each spp. downloaded?)
table(cws_gb2$species_code) # lost over half of the SPGR clips

## Add SPGR clips from CWS-ONT projects with SPGR:
# (Project IDs found by first filtering CWS-ONT projects then cross-checking on WT Data Discover which ones had SPGR audio data)
##Birds of James Bay Lowlands 2021 (885), Boreal Shield Lowlands Transition 2022 (1310), Winisk and Sutton River Systems 2022-2023 (1330), Ring of Fire Crescent Region 2024 (4207), 
## Niskibi-Severn 2025 (3887), Sachigo and Severn River Systems 2023-2024 (3045), Akimiski Island 2022 (1313), Boreal FMUs 2022 (1449), Atlas Northern River Trips 2022 (1472)
ont_spgr_proj <- wt_get_projects("ARU") %>%
  filter(project_id == "885" |
           project_id == "1310" |
           project_id == "1330" |
           project_id == "4207" |
           project_id == "3887" |
           project_id == "3045" |
           project_id == "1313" |
           project_id == "1449" |
           project_id == "1472")

## Download ont spgr projects
ont_spgr_tags <- lapply(ont_spgr_proj$project_id, function(pid) {
  tryCatch(
    wt_download_report(
      project_id = pid,
      sensor_id  = "ARU",
      report     = "tag"
    ),
    error = function(e) {
      message("Skipping project ", pid, ": ", e$message)
      NULL
    }
  )
})


## Bind into df and isolate SPGR tags
ont_spgr <- bind_rows(ont_spgr_tags)
ont_spgr <- ont_spgr %>% 
  filter(species_code == "SPGR")

## Remove any rows that don't have clip or spectrogram URLs
ont_spgr <- ont_spgr %>%
  filter(if_all(c(spectrogram_url, clip_url), ~ !is.na(.)))



## Download target species audio files (one clip at a time)
ont_spgr_rec <- lapply(seq_len(nrow(ont_spgr)), function(i) {
  tryCatch(
    {
      wt_download_media(
        input  = ont_spgr[i, ],
        output = "data/wildtrax_download_aru/audio/CWS_gamebirds",
        type   = "tag_clip_audio"
      )
      TRUE
    },
    error = function(e) {
      message("Skipping clip ", i, ": ", e$message)
      FALSE
    }
  )
})
### Many did not download - isolate projects that did download = 3887, 4207
ont_spgr <- ont_spgr %>% filter(project_id == "3887"| 
                                  project_id == "4207")

## Only 27 additional SPGR clips downloaded (total 105). Other CWS regions only have a few more detections, so it wouldn't reach threshold anyway - proceed 

## Add ont_spgr to cws_gb2
cws_gb2 <- bind_rows(cws_gb2, ont_spgr)

#### Run HawkEars (using python) to ID clips ####
## See HawkEars documentation (https://github.com/jhuus/HawkEars) for full installation and implementation instructions.
## HawkEars is coded in Python and run via command line. As such, commands need to either be executed using reticulate functions or system2 functions

## Create venv (if not already done)
# virtualenv_create("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/Python")

# Activate the correct venv 
use_virtualenv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/Python", required = TRUE)

# Check that Python environment is configured properly
py_config() 

## Install HawkEars and set up env (already done)
# Install HawkEars
# py_install("HawkEars", pip = TRUE)
# 
# # Uninstall torch, torchaudio, and torchvision (no uninstall pip function in R, so call python using system2 and a python executable)
# system2(reticulate::py_exe(), c("-m", "pip", "uninstall", "torch", "torchaudio", "torchvision"))
# 
# ## Reinstall correct versions of these packages
# 
# py_install(
#   packages = c(
#     "torch==2.8.0",
#     "torchvision==0.23.0",
#     "torchaudio==2.8.0"
#   ),
#   pip = TRUE,
#   pip_options = "--index-url https://download.pytorch.org/whl/cu126"
# )
## Temporarily set working directory to data/download to initialize HawkEars in a git ignored data folder
# with_dir("data/HawkEars_download", system2("hawkears", "init"))

## Check HawkEars analysis options
# system2("hawkears", #use hawkears package
#         c("analyze", "--help")) # call analyze script and --help function

## Run HawkEars species verification on clips in CWS_gamebirds

# create an output directory
dir.create("data/wildtrax_download_aru/audio/CWS_gamebirds/he_output")

# Run HawkEars on all WT NWT game bird recordings - don't specify region, since clips are from all over Canada
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/wildtrax_download_aru/audio/CWS_gamebirds", # set input folder to recordings
             "-o", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/wildtrax_download_aru/audio/CWS_gamebirds/he_output", # set output folder
             "--recurse", # process sub-directories (none here, but important later)
             "-r", "csv" # specify output as csv
           ))) 

# Analyzed 2568 clips in 8min14sec

#### Initial check of HawkEars output ####
he_cws <- read.csv("data/wildtrax_download_aru/audio/CWS_gamebirds/he_output/scores.csv")

## should be 2568 unique recordings
length(unique(he_cws$recording)) # 1857 - HE didn't detect a bird in every recording (recall)

## filter for target species only
he_gb <- he_cws %>% 
  filter(name %in% tar_spp) ## 1372 gamebird calls ID'd by HE
length(unique(he_cws$recording)) # 1857 - so only one call detected per clip

glimpse(he_gb)

## rename he_gb column names for clarity
colnames(he_gb) <- c("clip_name", "he_spp_ID", "start_time", "end_time", "conf_score")

## What is the total range of confidence scores?
summary(he_gb$conf_score) ## 0.701-0.992 


table(he_gb$he_spp_ID)
# ROPT RUGR SPGR STGR WIPT 
#  39  839  133   28  333 

# Combine HawkEars score range per species
he_scores_spp <- he_gb %>% group_by(he_spp_ID) %>% summarise(
  min_score = min(conf_score),
  mean_score = mean(conf_score),
  max_score = max(conf_score)
)

# HawkEars confidence scores a quite high for all game bird species. How do they compare to the WildTrax tags?
# 
# A field key is needed to match HawkEars IDs to the manual tags in nt_gb. When the WT recordings were downloaded, they were named based on these fields in nt_gb:
# (organization)_(location)_(recording_date_time)__(species_code)__(individual_order)__(detection_time)
# 
# We also want to select only relevant column in cws_gb


## Add clip name to cws_gb2
cws_gb2$clip_name <- with(cws_gb2, paste0(
  organization, "_",
  location, "_",
  format(as.POSIXct(recording_date_time), "%Y%m%d_%H%M%S"),
  "__",
  species_code,
  "__",
  individual_order,
  "__",
  gsub("\\.", "_", detection_time)
))


## Select relevant columns (move clip_name to first column)
gb2 <- cws_gb2 %>% 
  select(clip_name, organization, location, recording_date_time, species_code, individual_order, detection_time, vocalization, recording_id, tag_id)

length(unique(gb2$clip_name))

## Next, we combine the WildTrax tags to HawkEars gamebird output. We can measure performance by marking true and false positives and false negatives.
## Combine WT tags with HawkEars gamebird IDs to create a 'positives' df. Left join keeps all observations in he_gb
gb_pos <- left_join(he_gb, gb2, by = "clip_name")

## Create a observed column, where observed = 1 if the HE ID matches the species code, 0 if not
gb_pos$observed <- ifelse(gb_pos$he_spp_ID == gb_pos$species_code,
                          yes = 1,
                          no = 0)
table(gb_pos$observed)

## Re-order gb_pos by he_spp_ID and observed
gb_pos <- gb_pos %>%  arrange(he_spp_ID, observed)

## True and false positives per species
spp_pos <- gb_pos %>% 
  group_by(he_spp_ID) %>% ## need to group by HawkEars species, not WT
  summarise(tp = sum(observed),
            fp = sum(observed == 0))

## Summarise results in a table. Summary stats:
##   
##   HawkEars confidence score: certainty of identification, NOT probability of true detection. Lower confidence scores on true positives indicate that the algorithm is right even when it's not certain. Think Dunning-Kruger effect - high confidence often correlated with low knowledge!
## Precision: The proportion of times HawkEars ID'd a species and was right ('accuracy')
## Recall: The proportion of times HawkEars detected a bird and correctly ID'd it ('sensitivity')
## F1: estimates how well HawkEars IDs calls relative to a human observer

## Create a species table with total number of clips, total number of clips with HawkEars IDs, and confidence scores
# total NWT gamebird clips
gb_tbl <- as.data.frame(table(cws_gb2$species_code))
## add total HawkEars IDs
gb_tbl <- left_join(gb_tbl, as.data.frame(table(he_gb$he_spp_ID)), by = "Var1")
## add HawkEars confidence scores
gb_tbl <- cbind.data.frame(gb_tbl, he_scores_spp)

## Rename columns and remove 2nd species column
colnames(gb_tbl) <- c("species_code", "total_wt_clips", "total_HawkEars_IDs", "species_code2", "HEscore_min", "HEscore_mean", "HEscore_max")
gb_tbl <- gb_tbl[ , c(1:3, 5:7)]

## Add true and false positives (first need to rename he_spp_ID)
colnames(spp_pos) <- c("species_code", "tp", "fp")
gb_tbl <- left_join(gb_tbl, spp_pos, by = "species_code") 

# Add false negatives (total_clips - true positives)
gb_tbl$fn <- gb_tbl$total_wt_clips - gb_tbl$tp


## Add precision, recall, and F1 scores
gb_tbl$precision <- gb_tbl$tp/(gb_tbl$tp + gb_tbl$fp)
gb_tbl$recall <- gb_tbl$tp/(gb_tbl$tp + gb_tbl$fn)
gb_tbl$F1 <- 2 * ((gb_tbl$precision * gb_tbl$recall)/(gb_tbl$precision + gb_tbl$recall))

gb_tbl

## Save table, but summarize as total clips, mean HE score, precision, recall, and F1. Rename for publishing
gb_tbl2 <- gb_tbl %>% 
  select("Species Code" = species_code, "No. recordings" = total_wt_clips, "Mean HawkEars confidence" = HEscore_mean, "Precision" = precision, "Recall" = recall, "F1 score" = F1)  

gb_tbl2
## save as csv in figures for formatting into a table
write.csv(gb_tbl2, "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/figures/HawkEars_performance_CWS_gamebirds_20260424.csv")


#### Plot the detection rate against HawkEars confidence scores (using code from Tseng et al. 2025) ####
coul <- brewer.pal(5, "Set1")

g <- ggplot(gb_pos, aes(x = conf_score,
                        y = observed,
                        group = he_spp_ID, # use the HawkEars spp code
                        colour = he_spp_ID)) +
  geom_point(size = 5, 
             alpha = 0.1) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = binomial),
            linewidth = 1.5,
            alpha = 0.7) +
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(0.7, 1), expand = c(0, 0), breaks = seq(0.7, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme_bw() +
  labs(x = "HawkEars confidence", 
       y = "True positive rate",
       colour = "Species") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  guides(colour = guide_legend(ncol = 4)) 

win.graph()
g

## Save plot
ggsave(plot = g,
       filename = "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/figures/CWS_recs_HE_gamebird_calibration_curves_20260424.jpeg",
       width = 24,
       height = 19,
       units = "cm",
       dpi = 300)
