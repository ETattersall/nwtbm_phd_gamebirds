## acoustic_data_processing_workflow.R




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
                      "reticulate", # enables coding in python
                      "av", # conversion of flac to wav files
                      "trillR" # allows clipping of wav files
                      ) 

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages) # note: wildrtrax and trillR won't install on fresh lab server. Investigate later
lapply(list.of.packages, require, character.only = TRUE)



#### Run HawkEars (using python) to ID clips ####
## See HawkEars documentation (https://github.com/jhuus/HawkEars) for full installation and implementation instructions.
## HawkEars is coded in Python and run via command line. As such, commands need to either be executed using reticulate functions or system2 functions

### Create Python venv (if not already done) - make sure this is in the gitignored Python folder
virtualenv_create("/home/tatterer/Python/hawkears-venv")

# Activate the correct venv 
use_virtualenv("/home/tatterer/Python/hawkears-venv", required = TRUE)

# Check that Python environment is configured properly
py_config() 



## Install HawkEars and set up env (already done)
# Install HawkEars
py_install("HawkEars", pip = TRUE)

# Uninstall torch, torchaudio, and torchvision (no uninstall pip function in R, so call python using system2 and a python executable)
system2(py_exe(), c("-m", "pip", "uninstall", "-y", "torch", "torchvision", "torchaudio"))

# Install PyTorch with CUDA 12.8 (FRESH server, Linux)
py_install(
  packages = c("torch", "torchvision", "torchaudio"),
  pip = TRUE,
  extra_index_url = "https://download.pytorch.org/whl/cu128"
)

## Note: these packages required the CUDA 13 toolkit to also be installed


### Initialize HawkEars in the virtual environment
system2(
  command = "/home/tatterer/Python/hawkears-venv/bin/hawkears",
  args = "init"
  )


## Run HawkEars on test folder - ENWA-O-01-01_2022_May

### Check input folder has required recordings

list.files(
  "/home/tatterer/nwtbm_phd_gamebirds/data/ENWA-O-01-01_2022_May",
  recursive = TRUE
)


# create an output directory
dir.create("data/ENWA-O-01-01_2022_May/he_output")

# Run HawkEars on one folder - 143 files, 4.75 GB
system2( # run command line prompt
  command = "/home/tatterer/Python/hawkears-venv/bin/hawkears", # run HawkEars python package from venv
  c("analyze", # run analyze script
  "-i", "/home/tatterer/nwtbm_phd_gamebirds/data/ENWA-O-01-01_2022_May", # set input folder to recordings
  "-o", "/home/tatterer/nwtbm_phd_gamebirds/data/ENWA-O-01-01_2022_May/he_output", # set output folder
  "--recurse", # process sub-directories (none here, but important later)
  "-r", "csv", # specify output as csv
  "--region", "CA-NT" # specifies the eBird region code
  )) 

### Processing 4.75 gb took 57 minutes on personal laptop

### Processing 4.75 gb took 4:54 minutes on the FRESH lab server




#### Check out output ####
list.files("data/ENWA-O-01-01_2022_May/he_output")

scores <- read.csv("data/ENWA-O-01-01_2022_May/he_output/scores.csv")

glimpse(scores)

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

