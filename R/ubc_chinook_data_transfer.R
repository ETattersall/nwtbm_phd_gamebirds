##############################################
## ubc_chinook_data_transfer.R
## This script uses the rglobus package to retrieve data from a Globus Collection
## This requires Globus Connect Personal to be installed on your local drive, and a collection location specified.
## See instructions here for Globus Connect Personal: https://www.globus.org/globus-connect-personal
## rglobus vignette here: https://mtmorgan.github.io/rglobus/articles/a_get_started.html
## Script created for the WildCo Lab by Erin Tattersall
## Started on 30 April 2026
##############################################

### Environment setup ###

## Install the rglobus package from github - original rglobus package only works on local computers
## Install FRESH lab's patched version - only need to install once
# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# remotes::install_github("UBC-FRESH/rglobus")

## Set environment authentication variables
Sys.setenv(GLOBUS_CLIENT_ID = "23c8e7f1-5105-423f-a15c-0eab962b0d9d")
Sys.setenv(HTTR2_OAUTH_REDIRECT_URL = "https://auth.globus.org/v2/web/auth-code")

# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("tidyverse",
                      "purr",
                      "stringr",
                      "here", # helps find project files (and set root directories)
                      "withr", # to temporarily change directories
                      "reticulate", #enables coding in python
                      "rglobus" #for sharing and transferring files via globus
) 

# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Open globus and complete login/consent (run my_collections to show URL)
my_collections()

### Search WildCo globus collections

wildco_collections <- collections("WildCo")
## A web page will open the first time you do this; you will need to authenticate Globus and give HuBMAP access permissions

wildco_collections # Specify the WildCo Lab collection (this is the collection housed on UBC Chinook)
gwildco <- wildco_collections %>% filter(display_name == "WildCo Lab")
gwildco


## List collection contents & drill down the path of the required directory
# If you already know your data path you can specify it, otherwise you can iteratively use globus_ls to drill down to the desired folder
# path to acoustic data on Chinook
chin_acoustic <- "Camera_Trap_Projects/Active Projects/NWTBMP/acoustic_data"
# List folder contents
globus_ls(.data = gwildco,
          path = chin_acoustic)

## look at one station within EDE data
globus_ls(gwildco,
          path = paste(chin_acoustic, "Edehzhie2021", "ENWA-O-01-01", sep = "/"))

### Access my collections - this shows you the local collections you have set up on your drive

my_collections()
my_acoustic <- my_collections("nwtbm_acoustic") ## nwtbm_acoustic is the collection on my local drive - this roots to the C drive

## Checking local collection data downloads
globus_ls(my_acoustic, "C/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download")

##### Share a single directory between collections ##### 
### Chinook to FRESH server
fresh <- my_collections("fresh01.01101.dev/jupyterhub05")

### Create a directory on fresh to transfer into (if not already done)
globus_ls(fresh, "nwtbm_phd_gamebirds")
# mkdir(fresh, "nwtbm_phd_gamebirds/data")

## set path to destination folder
dest_path <- "/home/tatterer/nwtbm_phd_gamebirds/data"

# specify source path as entire ENWA_2022_May subdirectory
source_path <- paste(chin_acoustic, "Edehzhie2021", "ENWA-O-01-01_2022_May", sep = "/")

## set the destination to the local directory
destination_path <- paste(dest_path, "ENWA-O-01-01_2022_May", sep = "/")


## Copy ENWA_2022_May directory
task <- copy(
  gwildco, fresh, # specify source and destination collections
  source_path, destination_path, # specify paths
  notify_on_succeeded = FALSE,
  recursive = TRUE
)
## note: add recursive = TRUE for directories

## Transfer of 4.75 GB took 6 minutes, 13 seconds
 
glimpse(task)
task_status(task)

## If you need to cancel the task
# task_cancel(task)

globus_ls(fresh, destination_path)

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
dir.create("data/Chinook_download/Edehzhie2021")
## Create the collection relative destination path (note: this path CANNOT contain characters like [\\\\/:*?"<>|\r\n]. Windows paths often include a ':' for the drive (C:/...))
# note that destination paths on Linux may also need to start with /
dest_path <- "C/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/Edehzhie2021"

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

### Save Edehzhie file manifest (save in Edehzhie destination directory - dest_path)
write.csv(manifest_df, paste(dest_path, "Edehzhie_May_filemanifest_chinook_local.csv", sep = "/"))


###### Test transfer using a file manifest #####

### Subset the file manifest to the batch of files to be processed

## For this test I will only transfer files from 20220509_120000 (from all stations)
may9_files <- manifest_df |>
  filter(str_detect(name, "_20220509_120000.flac"))

## Create a single transfer item for each file
transfer_items <- purrr::map_chr( ## must be a character vector
  seq_len(nrow(may9_files)),
  function(i) {
    transfer_item(
      source_path      = may9_files$source_path[i],
      destination_path = may9_files$destination_path[i],
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
  label = "Edehzhie May 2021 FLACs",
  verify_checksum = TRUE,    # integrity check
  preserve_timestamp = TRUE # optional, but often useful
)

glimpse(task_id)
task_status(task_id)

# task_cancel(task_id)


##### Troubleshooting bad destination paths ####
## Check incorrect file or directory names
illegal_pattern <- '[\\\\/:*?"<>|\r\n]'

bad_rows <- may9_files |>
  dplyr::filter(
    grepl(illegal_pattern, basename(destination_path))
  )

nrow(bad_rows) ## no bad rows - file names are okay

## check destination path
bad_paths <- may9_files |>
  dplyr::filter(grepl(illegal_pattern, destination_path))

nrow(bad_paths)
bad_paths$destination_path[1]

