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
                      "purrr",
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
globus_ls(fresh, "/srv/scratch/tatterer-scratch/data")

## Trying the new shared drive with more storage
globus_ls(fresh, "/srv/shared-data/tatterer-scratch/data")


## Add project directory (only once per project)
dir.create("/srv/shared-data/tatterer-scratch/data/ThaideneNene2022")


##### Creating a file manifest to transfer select files #####
### Purpose: Generate a manifest of select files to be transferred based on a filename pattern
## In this case, I want all audio files recorded in April or May
## Project 1: Edehzhie2021 project, which only has May recordings from 2022
## Project 2: Gameti2024 project - started recording in June in 2023, and late April in 2024 (download 2024 April and May)
## Project 3: Thaidene Nene 2022 - started recording Apr 1 2022. Want recordings from April and May 2022

### Create a source path to Edehzhie2021 (starting from Wildco Lab collection)
tdn_globus <- "Camera_Trap_Projects/Active Projects/NWTBMP/acoustic_data/ThaideneNene2022"


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


# List all files in ThaideneNene2022 using list_recursive_fast (still takes some time to search all subdirectories)
tdn_files <- list_recursive_fast(gwildco, tdn_globus)

glimpse(tdn_files)
head(tdn_files)

### Step 3:
# ------------------------------------------------------------
# Create a Globus transfer manifest for April - May acoustic files
# ------------------------------------------------------------

## Set the path to the destination directory where all files should be transferred to (including subdirectories)

## Create the collection relative destination path (note: this path CANNOT contain characters like [\\\\/:*?"<>|\r\n]. Windows paths often include a ':' for the drive (C:/...))
# note that destination paths on Linux may also need to start with /
dest_path <- "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022"

## Function for creating a normalized globus path
normalize_globus_path <- function(x) {
  sub("^/+", "/", x)
}


## Generate file manifest, containing all source and destination paths for the file transfer
manifest_df <- tdn_files |>
  
  # Keep only FLAC files (or .wav files, if relevant)
  dplyr::filter(stringr::str_ends(name, "\\.wav")) |>
  
  # Filter to April and May recordings
  dplyr::filter(
    stringr::str_detect(name, "_[0-9]{4}04|05[0-9]{2}_")
  ) |>
  
  dplyr::mutate(
    
    # Globus requires collection-relative absolute paths
    source_path = paste0("/", sub("^/+", "", rel_path)), ## strips any leading slashs in rel_path and adds exactly one back
    
    # Extract station directory from collection-relative path
    station = sub(
      paste0("^", tdn_globus, "/([^/]+)/.*"),
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


## How much data total are in the file manifest? In GB
sum(manifest_df$size) / 1024^3 # 7891.8 GB - 7.9 TB


## the scratch directory has 5 - 10 TB of space

## Divide the manifest into smaller batches (no longer needed, since scratch size increased )

# ## Define batch size (size columns is in bytes)
# batch_size_bytes <- 258 * 1024^3
# 
# ## Generate batch_id based on amount of data desired in each batch
# manifest_batched <- manifest_df |>
#   arrange(source_path) |>   # or station, date, source_path
#   mutate(
#     cumulative_size = cumsum(as.numeric(size)),
#     
#     batch_id = floor((cumulative_size - 1) / batch_size_bytes) + 1
#   )
# 
# glimpse(manifest_batched)
# table(manifest_batched$batch_id) # 2 batches, between 15 095 - 13 067 files


### Save TDN file manifest (save in Edehzhie destination directory - dest_path)
write.csv(manifest_df, "/home/tatterer/nwtbm_phd_gamebirds/data/ThaideneNene_Apr-May_filemanifest_chinook_fresh-scratch.csv")

# ## Divide the manifest into 4 parts based on batch_id
# gam_batch1 <- manifest_batched[manifest_batched$batch_id == 1, ]
# gam_batch2 <- manifest_batched[manifest_batched$batch_id == 2, ]


###### Data transfer using a file manifest #####
## Restart here after processing a batch in HawkEars
## Confirm globus is running
my_collections()

### Transfer files one batch at a time

## Create a single transfer item for each file
transfer_items_batch <- purrr::map_chr( ## must be a character vector
  seq_len(nrow(manifest_df)),
  function(i) {
    transfer_item(
      source_path      = manifest_df$source_path[i],
      destination_path = manifest_df$destination_path[i],
      recursive = FALSE
    )
  }
)

class(transfer_items_batch)


## Submit one Globus transfer task for each batch

task_batch <- transfer(
  source      = gwildco,
  destination = fresh,
  transfer_items = transfer_items_batch,
  label = "TDN wav files 1",
  verify_checksum = TRUE,    # integrity check
  preserve_timestamp = TRUE # optional, but often useful
)

glimpse(task_batch)
task_status(task_batch)

### If transfer needs to be terminated (e.g., if scratch runs out of disk space)
task_cancel(task_batch)

### Confirm files were successfully transferred by listing contents of destination
globus_ls(fresh, dest_path)



######## Run HawkEars (using python) to ID clips ########

### HawkEars setup (only run once)
## See HawkEars documentation (https://github.com/jhuus/HawkEars) for full installation and implementation instructions.
## HawkEars is coded in Python and run via command line. As such, commands need to either be executed using reticulate functions or system2 functions

# ### Create Python venv (if not already done) - make sure this is in the gitignored Python folder
# virtualenv_create("/home/tatterer/Python/hawkears-venv")
# 
# # Activate the correct venv 
# use_virtualenv("/home/tatterer/Python/hawkears-venv", required = TRUE)
# 
# # Check that Python environment is configured properly
# py_config() 
# 
# 
# 
# ## Install HawkEars and set up env (already done)
# # Install HawkEars
# py_install("HawkEars", pip = TRUE)
# 
# # Uninstall torch, torchaudio, and torchvision (no uninstall pip function in R, so call python using system2 and a python executable)
# system2(py_exe(), c("-m", "pip", "uninstall", "-y", "torch", "torchvision", "torchaudio"))
# 
# # Install PyTorch with CUDA 12.8 (FRESH server, Linux)
# py_install(
#   packages = c("torch", "torchvision", "torchaudio"),
#   pip = TRUE,
#   extra_index_url = "https://download.pytorch.org/whl/cu128"
# )
# 
# ## Note: these packages required the CUDA 13 toolkit to also be installed
# 
# 
## upgrade HawkEars (when new versions are released)
# system("pip install --upgrade hawkears")

# ### Initialize HawkEars in the virtual environment - this will download the HawkEars recordings and yaml directories. If using git tracking, add these to .gitignore
# system2(
#   command = "/home/tatterer/Python/hawkears-venv/bin/hawkears",
#   args = "init"
#   )


#### Run HawkEars on current batch in scratch directory ####
## Make sure output directories are set to the correct batch number!!

### Check input folder has required recordings

list.files(
  "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022",
  recursive = TRUE
)

length(list.files(
  "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022",
  recursive = TRUE
)) ## 148 220

#### Dividing TDN recordings into batches for HawkEars processing ####
## Encounter CUDA Launch Timeout Error what processing full dataset - suggesting I'm processing too much data at once
## Need to batch process TDN recordings through HawkEars - 32 000 recordings were successfully processed before timeout encountered
## Try dividing into 5 batches - batches based on sub-directory, so will have uneven number of files

# ## List folders in scratch (station folders, not individual recordings)
# scratch_folders <- list.files(dest_path, full.names = TRUE)
# 
# head(scratch_folders)
# 
# # Number of batches
# n_batches <- 5
# 
# # Create batch labels (roughly equal distribution)
# batch_ids <- cut(seq_along(scratch_folders), breaks = n_batches, labels = FALSE)
# 
# 
# # Loop through batches
# for (i in 1:n_batches) {
#   batch_dir <- file.path(dest_path, paste0("batch", i))
#   
#   if (!dir.exists(batch_dir)) {
#     dir.create(batch_dir)
#   }
#   
#   batch_folders <- scratch_folders[batch_ids == i]
#   
#   # Copy folders
#   file.copy(batch_folders, batch_dir, recursive = TRUE)
#   
#   # Remove originals after successful copy
#   unlink(batch_folders, recursive = TRUE)
# }
# 
# ## Check number of files in each batch, and that none went missing
# length(list.files(
#   "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022",
#   recursive = TRUE
# )) ## all present
# 
# length(list.files(
#   "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022/batch1",
#   recursive = TRUE
# )) ## 77 185 - more than half the audio...
# 
# length(list.files(
#   "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022/batch5",
#   recursive = TRUE
# )) ## batches 2-5 have between 17 184 - 18 437
# 
# 
# ## Check folders in output directory
# list.files("/home/tatterer/he_output/ThaideneNene2022")
# 
# ## remove those folders
# unlink("/home/tatterer/he_output/ThaideneNene2022/batch1", recursive = TRUE)
# unlink("/home/tatterer/he_output/ThaideneNene2022/batch2_full", recursive = TRUE)
# 

# Run HawkEars on a single batch of Chinook data
system2( # run command line prompt
  command = "/home/tatterer/Python/hawkears-venv/bin/hawkears", # run HawkEars python package from venv
  c("analyze", # run analyze script
  "-i", "/srv/shared-data/tatterer-scratch/data/ThaideneNene2022/batch5", # set input folder to recordings
  "-o", "/home/tatterer/he_output/ThaideneNene2022/batch5", # set output folder (will create one if it doesn't exist)
  "--recurse", # process sub-directories
  "-r", "csv", # specify output csv
  "--region", "CA-NT" # specifies the eBird region code
  )) 

### Processing 4.75 gb took 57 minutes on personal laptop

### Processing 4.75 gb took 4:54 minutes on the FRESH lab server
### Processing 492 gb took 6:19:46 on FRESH lab server for batch 1, 5:50:29 for batch 2

### Running HawkEars on TDN batch1 - completed in 41:51:30 (77 185 wav files)
## TDN batch2 - completed in 09:06:06 (18 437 wav files)
## TDN batch3 - completed in 8:51:37
## TDN batch4 - completed in 9:30:44 (18 210 wav files)
## TDN batch5 - completed in 9:30:17 (17 204 wav files)

## Quick check of output ####
list.files("/home/tatterer/he_output/ThaideneNene2022")


scores <- read.csv("/home/tatterer/he_output/ThaideneNene2022/batch1/scores.csv")

glimpse(scores) ## 1 599 927 detections in total (1.6 million)
summary(scores)
table(scores$name)
head(scores$recording)
tail(scores$recording)

## Are all 148 220 recordings represented in the scores output? Likely not, given the cudaErrorLaunchTimeout error
length(unique(scores$recording)) # 45 010 recordings

### Batch HawkEars processing - combining outputs ####
# Set root path for TDN HawkEars outputs
tdn_he_root <- "/home/tatterer/he_output/ThaideneNene2022/"

## List scores.csv from each batch
score_files <- list.files(
  path = tdn_he_root,
  pattern = "^scores\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

## Combine all score files into 1 df
all_tdn_scores <- bind_rows(
  lapply(score_files, read.csv))


glimpse(all_tdn_scores) ## ~3 million detections
length(unique(all_tdn_scores$recording)) ## 84 821 recordings - so not all recordings contained detections. Many recordings also returned the error 'Invalid audio duration: 0.0 seconds'


### Resetting for next batch - clearing scratch drive ####

## 1. Double check files in scratch to note where next batch needs to start
scratch_files <- list.files(dest_path, recursive = TRUE)
length(scratch_files) 


## 2. If some files failed - Add missing files to file manifest for next batch
## Find the files that weren't transferred from batch1 and add them to batch2
## Isolate the relative path in batch1 by removing the prefix
# path_prefix <- "/srv/scratch/tatterer-scratch/data/ThaideneNene2022/"
# manifest_df <-
#   manifest_df %>%
#   dplyr::mutate(
#     rel_dest_path = sub(paste0("^", path_prefix), "", destination_path)
#   )
# 
# # ## Compare scratch_files to rel_dest_path and find files that don't match
# failed_files <- manifest_df |>
#   dplyr::filter(!(rel_dest_path %in% scratch_files))
# 
# ## Remove the rel_dest_path column and then add these rows to tdn_batch2
# failed_files <- failed_files[ , 1:7]
# 
# tdn_batch2 <- failed_files
# 
# ## 3. Remove the project directory from scratch
unlink(dest_path, recursive = TRUE, force = TRUE)
list.files(dest_path)
list.files("/srv/scratch/tatterer-scratch/data") ## Confirm directory deleted
# 
# ## 4. Recreate the project directory in scratch
# dir.create(dest_path, recursive = TRUE)

#### Reset after a full project ####
## 1. Download all batch output files to local drive + backup location - transferring between local endpoints requires a Globus subscription, so this has to be done manually
# Edehzhie batch4, Gameti: I also downloaded Audacity labels for each clip, but don't yet know how to transfer these (9606 text files) to my local drive

## 2. Clean up the project-specific batch file manifests in R environment
rm(tdn_globus, tdn_files, tdn_batch2, tdn_he_root)
rm(task_batch)
rm(transfer_items_batch)
