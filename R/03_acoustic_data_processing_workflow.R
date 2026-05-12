#########################################################
### acoustic_data_processing_workflow.R
### This script includes the full workflow for processing raw acoustic data with HawkEars
### Input: Data stored on the UBC Chinook server (retrieved using rglobus)
### Output: HawkEars detections of gamebirds, including confidence scores for each detection
### For full description of rglobus installation, use, and Chinook data transfers, see ubc_chinook_data_transfer.R
### Created by Erin Tattersall on May 8 2026
##########################################################


#### Environment set-up ####
## Set environment authentication variables

Sys.setenv(GLOBUS_CLIENT_ID = "23c8e7f1-5105-423f-a15c-0eab962b0d9d")
Sys.setenv(HTTR2_OAUTH_REDIRECT_URL = "https://auth.globus.org/v2/web/auth-code")

# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("tidyverse",
                      "purr",
                      "stringr",
                      "rglobus",
                      "here", # helps find project files (and set root directories)
                      "withr", # to temporarily change directories
                      "reticulate" # enables coding in python
                      ) 

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages) # note: wildrtrax and trillR won't install on fresh lab server. Investigate later
lapply(list.of.packages, require, character.only = TRUE)

######## rglobus: Generating file manifest and transferring data ##########
## Open globus and complete login/consent (run my_collections to show URL)
my_collections()

### Search WildCo globus collections

wildco_collections <- collections("WildCo")

## Save the WildCo Lab collection as an environment variable for your source collection
wildco_collections # Specify the WildCo Lab collection (this is the collection housed on UBC Chinook)
gwildco <- wildco_collections %>% filter(display_name == "WildCo Lab")
gwildco


# List folder contents for NWT acoustic data (should show downloaded acoustic projects)
globus_ls(.data = gwildco,
          path = "Camera_Trap_Projects/Active Projects/NWTBMP/acoustic_data")

### Access my collections - this shows you the local collections you have set up on your drive
## Save an environment variable for your destination collection
## note: globus endpoints may need to be activated locally
## on fresh server terminal:
## cd globusconnectpersonal-3.2.8
## ./globusconnectpersonal -start

my_collections()
fresh <- my_collections("fresh-acoustic")

## Checking local collection data downloads
globus_ls(fresh)


### Create a temp-data folder to transfer data into from Globus
mkdir(fresh, "temp-data")

##### Creating a file manifest to transfer select files #####
### Purpose: Generate a manifest of select files to be transferred based on a filename pattern
## In this case, I want all audio files recorded in April or May
## First tested for the Edehzhie2021 project, which only has May recordings from 2022


### Create a source path to Edehzhie2021 (starting from Wildco Lab collection)
ede_globus <- "Camera_Trap_Projects/Active Projects/NWTBMP/acoustic_data/Edehzhie2021"


## Step 1:
# ------------------------------------------------------------
# List contents of a Globus directory
# Wrapper kept intentionally:
# - simplifies traversal code
# - allows future pagination / logging upgrades
# - currently just forwards to globus_ls()

# ------------------------------------------------------------
globus_ls_all <- function(collection, path) {
  
  # Directly return directory listing
  # This client returns all entries in one call
  globus_ls(collection, path = path)
}

## Step 2:
# ------------------------------------------------------------
# Recursively list all files under a root path
# using an iterative breadth-first search
# ------------------------------------------------------------
list_recursive_fast <- function(collection, root_path) {
  
  # Queue of directories still to be processed
  queue <- root_path
  
  # List to store data frames of files
  out_files <- list()
  
  # Counter for optional progress messages
  dir_count <- 0
  
  while (length(queue) > 0) {
    
    # Pop first directory from queue
    current <- queue[[1]]
    queue <- queue[-1]
    dir_count <- dir_count + 1
    
    # Optional progress message every 500 directories
    if (dir_count %% 500 == 0) {
      message("Processed ", dir_count, " directories…")
    }
    
    # List current directory contents
    contents <- globus_ls_all(collection, current)
    
    # Skip empty directories
    if (nrow(contents) == 0) next
    
    # --------------------
    # Files
    # --------------------
    files <- contents |>
      dplyr::filter(type == "file") |>
      dplyr::mutate(
        # Preserve full collection-relative path
        rel_path = file.path(current, name)
      )
    
    if (nrow(files) > 0) {
      out_files[[length(out_files) + 1]] <- files
    }
    
    # --------------------
    # Subdirectories
    # --------------------
    dirs <- contents |>
      dplyr::filter(type == "dir")
    
    if (nrow(dirs) > 0) {
      queue <- c(
        queue,
        file.path(current, dirs$name)
      )
    }
  }
  
  # Combine all file records into one data frame
  dplyr::bind_rows(out_files)
}


# List all files in Edehzhie2021 using list_recursive_fast (still takes some time to search all subdirectories)
ede_files <- list_recursive_fast(gwildco, ede_globus)

glimpse(ede_files)

### Step 3:
# ------------------------------------------------------------
# Create a Globus transfer manifest for May FLAC files
# ------------------------------------------------------------

## Set the path to the destination directory where all files should be transferred to (including subdirectories)
# In this test, it will be in my local collection - create the directory if it doesn't exist

mkdir(fresh, "temp-data/Edehzhie2021")

## Create the collection relative destination path (note: this path CANNOT contain characters like [\\\\/:*?"<>|\r\n]. Windows paths often include a ':' for the drive (C:/...))
# note that destination paths on Linux may also need to start with /
dest_path <- "/temp-data/Edehzhie2021"

## Function for creating a normalized globus path
normalize_globus_path <- function(x) {
  sub("^/+", "/", x)
}


## Generate file manifest, containing all source and destination paths for the file transfer
manifest_df <- ede_files |>
  
  # Keep only FLAC files
  dplyr::filter(stringr::str_ends(name, "\\.flac")) |>
  
  # Filter to May recordings
  dplyr::filter(
    stringr::str_detect(name, "_[0-9]{4}05[0-9]{2}_")
  ) |>
  
  dplyr::mutate(
    
    # Globus requires collection-relative absolute paths
    source_path = paste0("/", sub("^/+", "", rel_path)), ## strips any leading slashs in rel_path and adds exactly one back
    
    # Extract station directory from collection-relative path
    station = sub(
      paste0("^", ede_globus, "/([^/]+)/.*"),
      "\\1",
      rel_path
    ),
    
    # Construct destination path:
    #   dest_path / station / filename
    destination_path = file.path(
      dest_path,
      station,
      name
    )
  ) |>
  
  ## Ensure source and destination paths are globus friendly
  dplyr::mutate(
    source_path = normalize_globus_path(source_path),
    destination_path = normalize_globus_path(destination_path)
  ) |>
  # Re-arrange and select relevant columns
  dplyr::select(station, name, size, rel_path, source_path, destination_path)


glimpse(manifest_df)
class(manifest_df)

### Create a batched manifest divided into batches of 80 GB each
## How much data total are in the EDE May file manifest? In GB
sum(manifest_df$size) / 1024^3 #1944.48 GB

## my fresh container has max 97 GB available space, and the scratch has 492 GB

## If I divide the data into 95 GB batches, I would need to transfer between temp data -> scratch 5 times, run that batch through HawkEars, and repeat that 4 times

## Define batch size (size columns is in bytes)
batch_size_bytes <- 95 * 1024^3


manifest_batched <- manifest_df |>
  arrange(source_path) |>   # or station, date, source_path
  mutate(
    cumulative_size = cumsum(size),
    
    batch_id = floor((cumulative_size - 1) / batch_size_bytes) + 1
  )

glimpse(manifest_batched)
table(manifest_batched$batch_id) # 21 batches, between 854 - 2577 files. 
## Divides nicely into 7 groups of 3 batches, Or 4 groups of 5 batches and 1 of 1
## (Run HawkEars 7 times vs 5)


### Save Edehzhie file manifest (save in Edehzhie destination directory - dest_path)
write.csv(manifest_batched, "/home/tatterer/nwtbm_phd_gamebirds/data/Edehzhie_May_filemanifest_chinook_fresh-temp-data.csv")

### Interim solution for being unable to transfer directly to the scratch directory
## I want to write a script that will:
## 1) Create a transfer item a batch of data (based on batch_id)
## 2) Transfer a single batch from source_path to destination_path using the globus transfer() function
## 3) Transfer that batch from destination_path to a corresponding subdirectory in the scratch directory
## 4) Delete that batch from destination_path
## 5) Repeat 1-4 for a new batch of data until the scratch directory has up to (but no more) than 490 GB of data

###### Data transfer using a file manifest #####

# ### Subset the file manifest to the batch of files to be processed
# 
# ## For this test I will only transfer files from 20220509_120000 (from all stations)
# may9_files <- manifest_df |>
#   filter(str_detect(name, "_20220509_120000.flac"))

## Create a single transfer item for each file
transfer_items <- purrr::map_chr( ## must be a character vector
  seq_len(nrow(manifest_df)),
  function(i) {
    transfer_item(
      source_path      = manifest_df$source_path[i],
      destination_path = manifest_df$destination_path[i],
      recursive = FALSE
    )
  }
)

class(transfer_items)


## Submit one Globus transfer task for all files from May 9 2022 at noon

task_id <- transfer(
  source      = gwildco,
  destination = my_acoustic,
  transfer_items = transfer_items,
  label = "Edehzhie May 2022 FLACs",
  verify_checksum = TRUE,    # integrity check
  preserve_timestamp = TRUE # optional, but often useful
)

glimpse(task_id)
task_status(task_id)

# task_cancel(task_id)

### Confirm files were successfully transferred by listing contents of destination
globus_ls(my_acoustic, dest_path)



######## Run HawkEars (using python) to ID clips ########
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


### Initialize HawkEars in the virtual environment - this will download the HawkEars recordings and yaml directories. If using git tracking, add these to .gitignore
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

