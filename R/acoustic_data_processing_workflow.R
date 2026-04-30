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
                      "reticulate", #enables coding in python
                      "seewave"#for working with audio files
                      ) 

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)









#### Run HawkEars (using python) to ID clips ####
## See HawkEars documentation (https://github.com/jhuus/HawkEars) for full installation and implementation instructions.
## HawkEars is coded in Python and run via command line. As such, commands need to either be executed using reticulate functions or system2 functions

### Create Python venv (if not already done)
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
system2("hawkears", #use hawkears package
         c("analyze", "--help")) # call analyze script and --help function

## Run HawkEars species verification on clips in CWS_gamebirds

# create an output directory
dir.create("data/Chinook_download/he_output")

# Run HawkEars on a single 10 minute recording
with_dir("data/HawkEars_download", ## Set working directory to run analyses where HawkEars scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download", # set input folder to recordings
             "-o", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/he_output", # set output folder
             "--recurse", # process sub-directories (none here, but important later)
             "-r", "audacity+csv", # specify output as csv
             "--region", "CA-NT" # specifies the eBird region code
           ))) 



###### Testing out converting the file to wav and clipping it using the seewave package

# find the audio file (remove he_output file)
aud <- list.files("data/Chinook_download")[1]

## Convert flac to wav (requires FLAC software to be downloaded)
wav2flac(file = aud,
         reverse = TRUE, # reverse = TRUE converts a flac to a wav file
         overwrite = FALSE) 
