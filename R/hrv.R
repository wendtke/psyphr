
# join parent hrv raw data
# filter to valid HRV
# PMH parent HRV data
# 03.31.2019



## ------ load packages -------------------

library(readxl)
library(writexl)
library(assertive)
library(tidyverse)
library(lubridate)


## --------- list of all hrv output files in working directory (both sheets) -----
files <- list.files("data/hrv/raw",
                    pattern = ".xlsx",
                    full.names = TRUE)




####### separate reading and cleaning functions for each sheet ##########

## ----------------- input and tidy each file within function ---------------------

read_hrv_file <- function(file){
  print(file) 
  read_xlsx(file, sheet = 1) %>% 
    gather(-`Segment Number`, 
           key = "segment", 
           value = "value") %>% 
    spread(key = `Segment Number`, 
           value = value) %>% 
    select(-`End Event`, 
           -`Start Event`, 
           -`Manual Override`, 
           -`Threshold (volts)`) %>% 
    rename(segment = segment,
           start_time = `Start Time`,
           end_time = `End Time`,
           seg_length = `Segment Duration`,
           mean_hr = `Mean Heart Rate`,
           rsa = RSA,
           mean_ibi = `Mean IBI`,
           r_peaks = `# of R's Found`,
           resp_rate = `Respiration Rate`,
           resp_amp = `Respiration Amplitude`,
           resp_peak_freq = `Respiration Peak Frequency`,
           resp_power = `Respiration Power`,
           first_r_time = `First ECG R Time`,
           last_r_time = `Last ECG R Time`,
           r_r_duration = `First R to Last R Duration`,
           sdnn = SDNN,
           avnn = AVNN,
           rmssd = RMSSD,
           nn50 = NN50,
           pnn50 = pNN50) %>%
    mutate_all(as.numeric) %>% 
    mutate(file_name = sub(".xlsx", "", basename(file)))
  }



## -------- reformat editing stat sheet within function ----------------------

read_edit_sheet <- function(file){
  print(file) 
  read_xlsx(file, sheet = 8) %>% 
    gather(-`Segment Number`, 
           key = "segment", 
           value = "value") %>% 
    spread(key = `Segment Number`, 
           value = value) %>% 
    rename(segment = segment,
           ecg_sec_cut = `ECG : Seconds Removed`,
           ecg_perc_cut = `ECG : Percentage Removed`,
           ecg_sec_est = `ECG : Seconds Estimated`,
           ecg_perc_est = `ECG : Percentage Estimated`,
           resp_sec_cut = `Resp : Seconds Removed`,
           resp_perc_cut = `Resp : Percentage Removed`,
           resp_sec_est = `Resp : Seconds Estimated`,
           resp_perc_est = `Resp : Percentage Estimated`,
           total_r_peaks = `Total Peaks`,
           norm_r_peaks = `Normal Peaks`,
           norm_r_perc = `% Normal Peaks`,
           est_r_peaks = `Estimated Peaks`,
           est_r_peaks_perc = `% Estimated Peaks`,
           art_r_peaks = `Artifact Peaks`,
           art_r_peaks_perc = `% Artifact Peaks`,
           dur_est_r_r = `Duration of Estimated R-R Intervals`,
           est_r_r_perc = `% of Estimated R-R Intervals`) %>%
    mutate_all(as.numeric) %>% 
    mutate(file_name = sub(".xlsx", "", basename(file)))
  }



############ read files separately ############


## -------------------- read in first HRV sheet -----------------------------

hrv_file <- read_hrv_file(files[1])

## --------------------- read in first HRV edit info -----------------------

hrv_edits <- read_edit_sheet(files[1])



############### map files separately ##########


## ------- run all files through read_hrv_file function and map to list ------

all_hrv_files <- map(files, read_hrv_file)


## ------ run all files through read_edit_sheet function and map to list ------

all_hrv_edits <- map(files, read_edit_sheet)



############### bind rows of each mapped list ###########


## -------------- bind files from list of all hrv sheets -------------------

parent_hrv <- bind_rows(all_hrv_files)

## --------------- bind files from list of all edit sheets ---------------------

parent_hrv_edits <- bind_rows(all_hrv_edits)




############################## clean up final tbls ###########################


## ------ reorder segments, separate file_name, and reorder columns in hrv sheets ---

parent_hrv <- parent_hrv %>% 
  arrange(file_name, segment) %>% 
  separate(col = file_name, 
           into = c("family", "individual", "task"),
           sep = "_") %>% 
  select(family, individual, 
         task, segment, 
         seg_length, start_time, end_time,
         rsa, mean_hr, 
         resp_peak_freq, resp_rate,
         everything())



## ---------- reorder segments and separate file_name in edit sheets -----------

parent_hrv_edits <- parent_hrv_edits %>% 
  arrange(file_name, segment) %>% 
  separate(col = file_name, 
           into = c("family", "individual", "task"),
           sep = "_") %>% 
  select(family, individual, 
         task, segment, 
         est_r_peaks_perc, est_r_peaks,
         norm_r_peaks, total_r_peaks,
         everything())


################################# JOIN ########################################


## -------------------- HRV PARENT DATA + HRV PARENT EDITS -----------------------



hrv <- right_join(parent_hrv, parent_hrv_edits,
           by = c("family", "individual", "task", "segment")) %>% 
  mutate_all(as.numeric)


############################ FILTER ############################################


## --------------- filtering hrv data based on segment validity ---------------
  

valid_hrv <- hrv %>% 
  filter(seg_length == 30) %>% 
  filter(resp_peak_freq >= 0.120 & resp_peak_freq <= 0.400) %>% 
  filter(est_r_peaks_perc <= 10.0)



############################## ADD VARIABLES ################################

## --------------------- means, sd, variance per task ----------------------

valid_hrv <- valid_hrv %>% 
  group_by(family, individual, task) %>% 
  mutate(rsa_mean_task = mean(rsa),
         rsa_sd_task = sd(rsa),
         rsa_var_task = var(rsa)) %>% 
  select(family, individual, task, segment, 
         rsa, rsa_mean_task, rsa_sd_task, rsa_var_task,
         mean_hr,
         everything())


describe(valid_hrv$rsa)

summary(valid_hrv$rsa)


######################## EXPORT EDITED DF TO XLS ############################

## ---------------- export parent HRV data to xls ------------------------------

write_xlsx(valid_hrv, "data/valid_parent_hrv.xlsx")




