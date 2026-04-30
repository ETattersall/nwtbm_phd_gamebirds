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
library(tidyverse)

## Install the rglobus package from github (if not already done)
# if (!requireNamespace("remotes", quiety = TRUE))
#   install.packages("remotes", repos = "https://CRAN.R-project.org")
# remotes::install_github("mtmorgan/rglobus", force = TRUE)

library(rglobus)


### Search WildCo globus collections

wildco_collections <- collections("WildCo")
## A web page will open; you will need to authenticate Globus and give HuBMAP access permissions

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
mkdir(my_acoustic, paste(local_acoustic, "ENWA_2022_May", sep = "/"))

## set the destination to the local directory
destination_path <- paste(local_acoustic, "ENWA_2022_May", sep = "/")

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
