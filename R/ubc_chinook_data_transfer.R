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

## Install the rglobus package from github (if not already done)
# if (!requireNamespace("remotes", quiety = TRUE))
#   install.packages("remotes", repos = "https://CRAN.R-project.org")
# remotes::install_github("mtmorgan/rglobus", force = TRUE)

# A list of the required packages (not all used in this script - copied from Chris's scripts)
list.of.packages <- c("tidyverse",
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


### Search WildCo globus collections

wildco_collections <- collections("WildCo")
## A web page will open the first time you do this; you will need to authenticate Globus and give HuBMAP access permissions

wildco_collections # Specify the WildCo Lab collection (this is the collection housed on UBC Chinook)
gwildco <- wildco_collections %>% filter(display_name == "WildCo Lab")

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
my_acoustic <- my_collections("nwtbm_acoustic") ## nwtbm_acoustic is the collection on my local drive - this roots to the C drive

# Specify the path to the nwtbm_phd_gamebirds data directory I want to copy files to (Chinook_download)
local_acoustic <- "C/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download"

## make a test directory in this folder
mkdir(my_acoustic,
      paste(local_acoustic, "test", sep = "/"))

globus_ls(my_acoustic, local_acoustic)


#### Copy data from Chinook to local ###
## Specify the data to be transferred - first acoustic file in first EDE station
source_path <- paste(chin_acoustic, "Edehzhie2021", "ENWA-O-01-01", "ENWA-O-01-01_20211002_182400.flac", sep = "/")
## Specify where it should be copied to (including filename)
destination_path <- paste(local_acoustic, "test", basename(source_path), sep = "/")

## Copy file over
task <- copy(
  gwildco, my_acoustic, # specify source and local collections
  source_path, destination_path, # specify paths
  notify_on_succeeded = FALSE
)
glimpse(task) # code = Accepted means the task has been added to the transfer task queue

# Check task status
task_status(task) ## SUCCEEDED!!!


### Now test on multiple files. It isn't yet possible to recursively select files based on a pattern (e.g., I only want Edehzhie files from May)
## I have copied all May files from one station (ENWA-O-01-01) into one folder on Chinook (ENWA_2022_May)
# specify source path as entire ENWA_2022_May subdirectory
source_path <- paste(chin_acoustic, "Edehzhie2021", "ENWA_2022_May", sep = "/")
# make a directory locally
mkdir(my_acoustic, paste(local_acoustic, "ENWA-O-01-01_2022_May", sep = "/"))

## set the destination to the local directory
destination_path <- paste(local_acoustic, "ENWA-O-01-01_2022_May", sep = "/")

## Copy ENWA_2022_May directory
task <- copy(
  gwildco, my_acoustic, # specify source and local collections
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




### Testing out looping through copy to transfer

## The goal is to recursively look through each station directory in a project and copy the flac files from April (if applicable) and May
## Starting with Edehzhie, which only has May recordings

## Create a path to Edehzhie2021 (starting from Wildco Lab collection)
ede_globus <- "Camera_Trap_Projects/Active Projects/NWTBMP/acoustic_data/Edehzhie2021"

ede_stations <- globus_ls(.data = gwildco,
          path = ede_globus) # lists all Edehzhie directories
## Select name column and specify a single station
ede_stn_dirs <- ede_stations %>% 
  select(name)
ede_stn_dirs[1,1]

## Create a list of all May recordings from one station
enwa_01_01_files <- globus_ls(.data = gwildco,
                              path = paste(ede_globus, "ENWA-O-01-01", sep = "/"))
enwa_01_01_may <- enwa_01_01_files %>% 
  filter(grepl(pattern = "_[0-9]{4}05[0-9]{2}", x = name)) %>% select(name) ## where the regex pattern searched = "_" + "[digits 0 - 9]{4 digits}" + "05 (i.e, May)" + "[digits 0-9]{2 digits}"
class(enwa_01_01_may) # tibble (it might need to be a list?)



#### Co- Pilot code to generate a May file manifest (not tested yet!)

library(rglobus)
library(dplyr)
library(purrr)
library(stringr)

# ------------------------------------------------------------
# Recursively list all files under ede_globus within gwildco
# ------------------------------------------------------------

list_recursive <- function(collection, path) {
  contents <- globus_ls(collection, path = path)
  
  files <- contents %>%
    filter(type == "file")
  
  dirs <- contents %>%
    filter(type == "dir")
  
  if (nrow(dirs) == 0) {
    return(files)
  }
  
  bind_rows(
    files,
    map_dfr(dirs$name,
            ~ list_recursive(collection,
                             file.path(path, .x)))
  )
}

# ------------------------------------------------------------
# Build manifest
# ------------------------------------------------------------

all_files <- list_recursive(gwildco, ede_globus)

may_manifest <- all_files %>%
  filter(str_ends(name, "\\.flac")) %>%
  filter(str_detect(name, "_[0-9]{4}05[0-9]{2}_")) %>%
  mutate(
    # collection-relative paths (DO NOT remove leading slash)
    source_path = file.path("/", path, name),
    
    station = basename(path),
    
    yyyymmdd = str_extract(name, "[0-9]{8}"),
    hhmmss   = str_extract(name, "(?<=_)[0-9]{6}(?=\\.flac$)"),
    date     = as.Date(yyyymmdd, "%Y%m%d"),
    year     = substr(yyyymmdd, 1, 4),
    month    = substr(yyyymmdd, 5, 6),
    day      = substr(yyyymmdd, 7, 8)
  ) %>%
  select(
    station,
    name,
    source_path,
    date,
    year,
    month,
    day,
    hhmmss,
    size
  ) %>%
  arrange(station, date, hhmmss)

# Inspect manifest
print(may_manifest)

# Optional: save manifest locally
write.csv(
  may_manifest,
  "Edehzhie2021_May_audio_manifest.csv",
  row.names = FALSE
)
