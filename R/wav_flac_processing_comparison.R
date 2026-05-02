########################################
## wav_flac_processing_comparison.R
## Comparing processing times for flac and wav files, including conversion time and HawkEars processing
## Also includes a comparison of HawkEars processing for 10min wav files vs 3min wav files
## Created by Erin Tattersall (with code from CWS, thanks to Eamon Riordan-Short)
## Started on 1 May 2026
########################################


#if you haven't already installed TrillR, you will need to run the next line
devtools::install_github("deanrobertevans/TrillR")

library(av)
library(TrillR)
library(reticulate)
library(tidyverse)
library(withr)
library(ggplot2)
library(RColorBrewer)

## testing HawkEars performance on flac vs wav files
## 1. 2 files (~ 70 MB as flac)
## 2. 33 files (1 GB)


##### Converting flac files to wav files (Eamon's code) ####
# --- USER SETTINGS ---
input_dir  <- "data/Chinook_download/file_format_test/flac"   # your folder containing FLACs (subfolders are okay)
output_dir <- "data/Chinook_download/file_format_test/wav" #Pick an output directory, be ready for big files!

# --- SCRIPT ---
# 1. Find all .flac files recursively
flac_files <- list.files(input_dir, pattern = "\\.flac$", recursive = TRUE, full.names = TRUE)

# Precompute normalized input directory
norm_input_dir <- normalizePath(input_dir, winslash = "/")

# Precompute normalized FLAC paths
norm_flac_files <- normalizePath(flac_files, winslash = "/")

# 2. Loop through each file and convert
## Run from start_time to Total execution time message
start_time <- Sys.time()

for (i in seq_along(norm_flac_files)) {
  
  flac <- norm_flac_files[i]
  
  rel_path <- sub(paste0("^", norm_input_dir, "/?"), "", flac)
  out_file <- file.path(output_dir, sub("\\.flac$", ".wav", rel_path))
  out_folder <- dirname(out_file)
  
  if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)
  
  # Skip if already converted
  if (file.exists(out_file)) next
  
  # Try converting, catch errors
  tryCatch({
    av_audio_convert(flac, out_file, verbose = FALSE)
  }, error = function(e){
    warning("Failed to convert ", rel_path, " — skipping. Error: ", e$message)
  })
  
  # Trigger garbage collection occasionally
  if (i %% 150 == 0) {
    gc()
    Sys.sleep(10)
  }
}

end_time <- Sys.time()
## Calculating elapsed time in seconds
elapsed <- difftime(end_time, start_time, units = "secs")
message("Total execution time: ", round(elapsed, 2), " seconds")

gc() # prints memory statistics
## Elapsed time:
## 70 MB: 1 second
## 1 GB: 15.72 seconds


## Check file size of flac files
flac_size_mb <- (file.size(dir(path = input_dir, full.names = TRUE))) * (2^-20) # need full.names = TRUE to return a file size
## file size is in bytes (binary) - convert to megabytes by multiplying by 2^-20
flac_size_mb ## each file = ~ 34 mb

## Check file size of wav files
wav_size_mb <- (file.size(dir(path = output_dir, full.names = TRUE))) * (2^-20)
wav_size_mb ## each file = ~ 100 mb

## wav files are 3 times as large


#### HawkEars processing time comparison ####
## Note - this requires a Python environment to already exist and HawkEars already installed
# Activate the correct venv 
use_virtualenv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/Python", required = TRUE)

# Check that Python environment is configured properly
py_config()


## flac files
## Run HawkEars (output saved in same folder)
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/flac", # set input folder to recordings
             "-r", "csv" # specify output as csv
           ))) 

## 70 MB: Total elapsed time = 1min 38 sec
## 1 GB: 15:54

## wav files
## Run HawkEars (output saved in same folder)
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/wav", # set input folder to recordings
             "-r", "csv" # specify output as csv
           )))


## Total elapsed time = 
## 70 MB: 1:27
## 1 GB: 18:14 - 2 MINUTES SLOWER!!!

## Do results differ? (compared with 1 GB)
sc_flac <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/flac/scores.csv")
sc_wav <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/wav/scores.csv")
# same number of total detections
length(unique(sc_flac$name)) # 43 unique species
flac_spp <- sc_flac %>% 
  group_by(name) %>% 
  summarize(flac_count = n())

length(unique(sc_wav$name)) # 43 uniques species
wav_spp <- sc_wav %>% 
  group_by(name) %>% 
  summarize(wav_count = n())

filetype_count <- left_join(flac_spp, wav_spp, by = "name") ## Identical detections

#### Calculate HawkEars processing time when files are clipped to 3min (testing 70 MB dataset only for speed)
## TrillR functions only work on wav files and use SOX software

#Read in SOX
setsox.exe("C:/Program Files (x86)/sox-14-4-2/sox.exe")

## get path to wav files
wav_files <- list.files(output_dir, pattern = "\\.wav$", recursive = TRUE, full.names = TRUE)
norm_wav_files <- normalizePath(wav_files, winslash = "/")
class(norm_wav_files)

## sox clips works off a df, so wrap norm_wav_files into a df
wav_df <- data.frame(
  file.path = norm_wav_files,
  stringsAsFactors = FALSE
)


## make new directory for clipped wave files
dir.create("data/Chinook_download/file_format_test/wav_clipped")


## Clip wav files to 3 minutes - output is same 2 wav files, but 3 min long instead of 10min long
sox.clips(wav_df, 
          out.path = file.path(getwd(), "data/Chinook_download/file_format_test/wav_clipped"),
          duration = list(start = 0, end = 180))

## Run through HawkEars (recall that 2 10 min wav files took 1min 27 sec)
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/wav_clipped", # set input folder to recordings
             "-r", "csv" # specify output as csv
           )))
## 16 seconds - so MUCH faster

## Compare outputs
he_10 <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/wav/scores.csv")
he_3 <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/wav_clipped/scores.csv")

## 36 obs vs 26

# same number of total detections
length(unique(he_10$name)) # 5 unique species
he_10_spp <- he_10 %>% 
  group_by(name) %>% 
  summarize(he_10_count = n())

length(unique(he_3$name)) # 4 uniques species
he_3_spp <- he_3 %>% 
  group_by(name) %>% 
  summarize(he_3_count = n())

he_10v3 <- left_join(he_10_spp, he_3_spp, by = "name") 


## Try clipping flac files with av_audio_output
norm_flac_files

## specify an output dir
dir.create("data/Chinook_download/file_format_test/flac_clipped")
output_flac <- "data/Chinook_download/file_format_test/flac_clipped"


### Function to batch clip flac files (keeps directory structure of input)
clip_flac_tree <- function(norm_flac_files, #vector of absolute file paths
                           input_root, #relative input directory
                           output_flac, #relative output directory
                           total_time,
                           suffix = NULL) {
  
  input_root <- normalizePath(input_root, winslash = "/", mustWork = TRUE)
  
  for (f in norm_flac_files) {
    
    f_norm <- normalizePath(f, winslash = "/", mustWork = TRUE)
    
    # Build relative path
    rel_path <- sub(paste0("^", input_root, "/?"), "", f_norm)
    
    # Construct output file path (and name output file)
    out_file <- file.path(
      output_flac,
      sub("\\.flac$", paste0(suffix, ".flac"), rel_path)
    )
    
    # Ensure output directory exists
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    
    # Skip if already clipped
    if (file.exists(out_file)) next
    
    # Clip file
    tryCatch({
      av_audio_convert(
        audio      = f_norm,
        output     = out_file,
        total_time = total_time
      )
    }, error = function(e) {
      warning("Failed to clip: ", f_norm, "\n", e$message, call. = FALSE)
    })
  }
}

## clip 2 flac files to 3min
clip_flac_tree(norm_flac_files = norm_flac_files, #vector of flac file paths
               input_root = input_dir, #flac folder input
               output_flac = output_flac, #flac_clipped folder output
               total_time = 180,
               suffix = "_3minclip")

## Run through HawkEars (recall that 2 10 min flac files took 1min 38 sec)
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/file_format_test/flac_clipped", # set input folder to recordings
             "-r", "csv" # specify output as csv
           )))
## 15 seconds

## Expect same results as he_3 - check
flac_3 <- read.csv(file.path(output_flac, "scores.csv")) ## confirmed - same output


##### Duration test - compare number of species detected with clips of varying lengths
## Use test directory of 11 flac files (ENWA-O-01-01, May 9-10 2022)
# specify input directory
input_dur <- "data/Chinook_download/duration_test/flac_10min"

## name input files
flac_10min <- list.files(input_dur, pattern = "\\.flac$", recursive = TRUE, full.names = TRUE)
norm_10min_flac <- normalizePath(flac_10min, winslash = "/")

## create an output directory
## specify an output dir
dir.create("data/Chinook_download/duration_test/clipped")
output_clipped <- "data/Chinook_download/duration_test/clipped"

# create a vector of clip durations in seconds (from 60 - 600), increasing by a minute each time
duration <- seq(60, 600, length.out = 10)


## Loop over the duration specified in duration to clip the input files to lengths 1 - 10min
## doesn't create a directory structure, but that's fine

for (d in duration) {
  
  message("Clipping ", d, " second files...")
  
  output_dir <- file.path(output_clipped, paste0("output_", d, "sec"))
  
  clip_flac_tree(
    norm_flac_files = norm_10min_flac,
    input_root      = input_dur,
    output_flac     = output_clipped,
    total_time      = d,
    suffix          = paste0("_", d, "sec")
  )
}

## Now run these clips through HawkEars
with_dir("data/HawkEars_download", ## Set working directory to run analyses where scripts are saved
         system2( # run command line prompt
           command = "hawkears", # run HawkEars python package
           c("analyze", # run analyze script
             "-i", "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/duration_test/clipped", # set input folder to recordings
             "-r", "csv" # specify output as csv
           )))
## 22min elapsed time
# Read in HawkEars results
he_dur <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/data/Chinook_download/duration_test/clipped/scores.csv")
glimpse(he_dur)

## Are all clips represented? should be 110
length(table(he_dur$recording)) ## 99 - so birds weren't detected in 11 recordings


## Count total number of detections per clip
he_det <- he_dur %>% group_by(recording, name) %>% 
  summarise(n_det = n())

## Create a clip duration column
he_det <- he_det %>% 
  mutate(recording_parts = strsplit(recording, "_")) %>% 
  mutate(
    location = sapply(recording_parts, `[`, 1),
    date     = as.Date(sapply(recording_parts, `[`, 2), format = "%Y%m%d"),
    time     = format(strptime(sapply(recording_parts, `[`, 3), "%H%M%S"), "%H:%M:%S"),
    duration = 
      as.numeric(sub("sec$", "", sapply(recording_parts, `[`, 4))))%>%
  select(-recording_parts)

max(he_det$n_det)

### Plot number of detections by clip

coul <- brewer.pal(12, "Set3")

g <- ggplot(he_det, aes(x = duration,
                        y = n_det,
                        group = name, # use the HawkEars spp code
                        colour = name)) +
  geom_point(size = 5, 
             alpha = 0.1) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = poisson),
            linewidth = 1.5,
            alpha = 1) +
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(60, 600), expand = c(0, 0), breaks = seq(60, 600, by = 60)) +
  scale_y_continuous(limits = c(1, 67)) + 
  theme_bw() +
  labs(x = "Clip duration (sec)", 
       y = "Number of detections",
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


### Save plot
ggsave(plot = g,
       filename = "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/figures/HawkEars_sppdetections_by_cliplength_20260501.jpeg",
       width = 40,
       height = 30,
       units = "cm",
       dpi = 300)

## Most lines continue to increase with increasing clip length

## Number of species per clip length 
## he_dur includes multiple detections per species - summarise he_det to count number of species
he_spp <- he_det %>% group_by(recording) %>% 
  summarise(n_spp = n())

max(he_spp$n_spp) ## max 6 species per clip

## Create column for clip duration, and initial 10 min recording name
he_spp <- he_spp %>% 
  mutate(recording_parts = strsplit(recording, "_")) %>% 
  mutate(
    location = sapply(recording_parts, `[`, 1),
    date     = as.Date(sapply(recording_parts, `[`, 2), format = "%Y%m%d"),
    time     = format(strptime(sapply(recording_parts, `[`, 3), "%H%M%S"), "%H:%M:%S"),
    duration = 
      as.numeric(sub("sec$", "", sapply(recording_parts, `[`, 4))),
    full_clip_name = paste(location, date, time, sep = "_")) %>% ## paste all parts of recording name except clip duration
  select(-recording_parts)

## Should be 11 full_clip_names
table(he_spp$full_clip_name)

### Plot species accumulation curve
coul <- brewer.pal(10, "Set3")

g2 <- ggplot(he_spp, aes(x = duration,
                        y = n_spp,
                        group = full_clip_name, # group by original recording
                        colour = full_clip_name)) +
  geom_point(size = 5, 
             alpha = 0.1) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = poisson),
            linewidth = 1.5,
            alpha = 1) +
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(60, 600), expand = c(0, 0), breaks = seq(60, 600, by = 60)) +
  scale_y_continuous(limits = c(0, 6)) + 
  theme_bw() +
  labs(x = "Clip duration (sec)", 
       y = "Number of species detected",
       colour = "Original Recording") +
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
g2


## without original recordings
g3 <- ggplot(he_spp, aes(x = duration,
                        y = n_spp)) +
  geom_point(size = 5, 
             alpha = 0.1) +
  geom_line(stat = "smooth",
            method = "glm", 
            se = FALSE, 
            method.args = list(family = poisson),
            linewidth = 1.5,
            alpha = 1) +
  scale_colour_manual(values = coul) +
  scale_x_continuous(limits = c(60, 600), expand = c(0, 0), breaks = seq(60, 600, by = 60)) +
  scale_y_continuous(limits = c(0, 6)) + 
  theme_bw() +
  labs(x = "Clip duration (sec)", 
       y = "Number of species detected") +
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
g3

## Save plots
ggsave(plot = g2,
       filename = "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/figures/HawkEars_speciesrichness_cliplength_rec_20260501.jpeg",
       width = 40,
       height = 30,
       units = "cm",
       dpi = 300)

ggsave(plot = g3,
       filename = "C:/Users/tatterer.stu/Desktop/nwtbm_phd_gamebirds/figures/HawkEars_speciesrichness_cliplength_20260501.jpeg",
       width = 40,
       height = 30,
       units = "cm",
       dpi = 300)
