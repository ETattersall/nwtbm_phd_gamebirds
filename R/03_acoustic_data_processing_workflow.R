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
globus_ls(fresh, "/srv/scratch/tatterer-scratch/data/Edehzhie2021")


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
# create the directory if it doesn't exist

mkdir(fresh, "/srv/scratch/tatterer-scratch/data/Edehzhie2021")

## Create the collection relative destination path (note: this path CANNOT contain characters like [\\\\/:*?"<>|\r\n]. Windows paths often include a ':' for the drive (C:/...))
# note that destination paths on Linux may also need to start with /
dest_path <- "/srv/scratch/tatterer-scratch/data/Edehzhie2021"

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

## the scratch directory has 490 GB space

## Divide the manifest into 4 parts

## Define batch size (size columns is in bytes)
batch_size_bytes <- 490 * 1024^3


manifest_batched <- manifest_df |>
  arrange(source_path) |>   # or station, date, source_path
  mutate(
    cumulative_size = cumsum(size),
    
    batch_id = floor((cumulative_size - 1) / batch_size_bytes) + 1
  )

glimpse(manifest_batched)
table(manifest_batched$batch_id) # 4 batches, between 9600 - 10 788 files


### Save Edehzhie file manifest (save in Edehzhie destination directory - dest_path)
write.csv(manifest_batched, "/home/tatterer/nwtbm_phd_gamebirds/data/Edehzhie_May_filemanifest_chinook_fresh-scratch.csv")

## Divide the manifest into 4 parts based on batch_id
ede_batch1 <- manifest_batched[manifest_batched$batch_id == 1, ]
ede_batch2 <- manifest_batched[manifest_batched$batch_id == 2, ] ## note that failed files from ede_batch1 were added to ede_batch2
ede_batch3 <- manifest_batched[manifest_batched$batch_id == 3, ]
ede_batch4 <- manifest_batched[manifest_batched$batch_id == 4, ]

###### Data transfer using a file manifest #####
## Restart here after processing a batch in HawkEars
## Confirm globus is running
my_collections()

### Transfer files one batch at a time

## Create a single transfer item for each file
transfer_items_batch2 <- purrr::map_chr( ## must be a character vector
  seq_len(nrow(ede_batch2)),
  function(i) {
    transfer_item(
      source_path      = ede_batch2$source_path[i],
      destination_path = ede_batch2$destination_path[i],
      recursive = FALSE
    )
  }
)

class(transfer_items_batch2)


## Submit one Globus transfer task for each batch

task_batch2 <- transfer(
  source      = gwildco,
  destination = fresh,
  transfer_items = transfer_items_batch2,
  label = "Edehzhie batch2 FLACs",
  verify_checksum = TRUE,    # integrity check
  preserve_timestamp = TRUE # optional, but often useful
)

glimpse(task_batch2)
task_status(task_batch2)

### If transfer needs to be terminated (e.g., if scratch runs out of disk space)
#task_cancel(task_batch2)

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
# ### Initialize HawkEars in the virtual environment - this will download the HawkEars recordings and yaml directories. If using git tracking, add these to .gitignore
# system2(
#   command = "/home/tatterer/Python/hawkears-venv/bin/hawkears",
#   args = "init"
#   )


#### Run HawkEars on current batch in scratch directory ####
## Make sure output directories are set to the correct batch number!!

### Check input folder has required recordings

list.files(
  "/srv/scratch/tatterer-scratch/data/Edehzhie2021",
  recursive = TRUE
)

list.files("/home/tatterer/he_output")

# create an output directory
dir.create("/home/tatterer/he_output/batch2")

# Run HawkEars on one Edehzhie batch (10 773 files, 492 GB)
system2( # run command line prompt
  command = "/home/tatterer/Python/hawkears-venv/bin/hawkears", # run HawkEars python package from venv
  c("analyze", # run analyze script
  "-i", "/srv/scratch/tatterer-scratch/data/Edehzhie2021", # set input folder to recordings
  "-o", "/home/tatterer/he_output/batch2", # set output folder
  "--recurse", # process sub-directories
  "-r", "csv", # specify output as csv
  "--region", "CA-NT" # specifies the eBird region code
  )) 

### Processing 4.75 gb took 57 minutes on personal laptop

### Processing 4.75 gb took 4:54 minutes on the FRESH lab server
### Processing 492 gb took 6:19:46 on FRESH lab server



## Quick check of output ####
list.files("/home/tatterer/he_output/batch1")

scores <- read.csv("/home/tatterer/he_output/batch1/scores.csv")

glimpse(scores)
summary(scores)
table(scores$name)

### Resetting for next batch ####

## 1. Double check files in scratch to note where next batch needs to start
scratch_files <- list.files(dest_path, recursive = TRUE)
length(scratch_files) ## 10 773 --> 15 files not transferred from ede_batch1
tail(scratch_files) ## last file: ENWA-O-09-05/ENWA-O-09-05_0+ -- so it wasn't just the last couple files on the manifest that were missed

## 2. Add missing files to file manifest for next batch
## Find the files that weren't transferred from ede_batch1 and add them to ede_batch2
## Isolate the relative path in ede_batch1 by removing the prefix
path_prefix <- "/srv/scratch/tatterer-scratch/data/Edehzhie2021/"
ede_batch1 <- 
  ede_batch1 %>% 
  dplyr::mutate(
    rel_dest_path = sub(paste0("^", path_prefix), "", destination_path)
  )

## Compare scratch_files to rel_dest_path and find files that don't match
failed_files <- ede_batch1 |>
  dplyr::filter(!(rel_dest_path %in% scratch_files))

## Remove the rel_dest_path column and then bind these rows to ede_batch2
failed_files <- failed_files[ , 1:8]

ede_batch2 <- bind_rows(failed_files, ede_batch2)

## 3. Remove the Edehzhie2021 directory from scratch
unlink(dest_path, recursive = TRUE, force = TRUE)
list.files(dest_path)
list.files("/srv/scratch/tatterer-scratch/data")## Confirm directory deleted

## 4. Recreate the Edehzhie2021 directory in scratch
dir.create(dest_path, recursive = TRUE)


