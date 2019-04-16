
# join parent raw eda data
# examine possible outliers/consider validity criteria
# PMH parent EDA data
# 04.06.2019

# load packages

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(assertive)



## --------- list of all eda output files in working directory (both sheets) --
files <- list.files("data/raw",
                    pattern = ".xlsx",
                    full.names = TRUE)



####### separate reading and cleaning functions for each sheet ##########

## ----------------- input and tidy each file within function -----------------

read_eda_file <- function(file){
  print(file)
  read_xlsx(file, sheet = 1) %>%
    gather(-`Segment Number`,
           key = "segment",
           value = "value") %>%
    spread(key = `Segment Number`,
           value = value) %>%
    select(-`End Event`,
           -`Start Event`) %>%
    rename(segment = segment,
           start_time = `Start Time`,
           end_time = `End Time`,
           seg_length = `Segment Duration`,
           total_scr = `Total SCRs`,
           er_scr = `ER-SCRs`,
           ns_scrs = `NS-SCRs`,
           tonic_scl = `Tonic SCL`,
           mean_sc = `Mean SC`,
           tonic_period = `Tonic Period`) %>%
    mutate_all(as.numeric) %>%
    mutate(file_name = sub(".xlsx", "", basename(file)))
}



## -------- reformat editing stat sheet within function ----------------------

read_edit_sheet <- function(file){
  print(file)
  read_xlsx(file, sheet = 3) %>%
    gather(-`Segment Number`,
           key = "segment",
           value = "value") %>%
    spread(key = `Segment Number`,
           value = value) %>%
    rename(segment = segment,
           eda_sec_cut = `EDA : Seconds Removed`,
           eda_perc_cut = `EDA : Percentage Removed`,
           eda_sec_est = `EDA : Seconds Estimated`,
           eda_perc_est = `EDA : Percentage Estimated`,
           resp_sec_cut = `Resp : Seconds Removed`,
           resp_perc_cut = `Resp : Percentage Removed`,
           resp_sec_est = `Resp : Seconds Estimated`,
           resp_perc_est = `Resp : Percentage Estimated`) %>%
    mutate_all(as.numeric) %>%
    mutate(file_name = sub(".xlsx", "", basename(file)))
}



############ read files separately ############


## -------------------- read in first eda sheet -----------------------------

eda_file <- read_eda_file(files[1])

## --------------------- read in first eda edit info -----------------------

eda_edits <- read_edit_sheet(files[1])



############### map files separately ##########


## ------- run all files through read_eda_file function and map to list ------

all_eda_files <- map(files, read_eda_file)


## ------ run all files through read_edit_sheet function and map to list ------

all_eda_edits <- map(files, read_edit_sheet)



############### bind rows of each mapped list ###########


## -------------- bind files from list of all eda sheets -------------------

parent_eda <- bind_rows(all_eda_files)

## --------------- bind files from list of all edit sheets --------------------

parent_eda_edits <- bind_rows(all_eda_edits)




############################## clean up final tbls ###########################


## ------ reorder segments, separate file_name, and reorder columns in eda

parent_eda <- parent_eda %>%
  arrange(file_name, segment) %>%
  separate(col = file_name,
           into = c("family", "individual", "task"),
           sep = "-") %>%
  select(family, individual,
         task, segment,
         seg_length, start_time, end_time,
         tonic_scl, total_scr,
         everything())



## ---------- reorder segments and separate file_name in edit sheets ---------

parent_eda_edits <- parent_eda_edits %>%
  arrange(file_name, segment) %>%
  separate(col = file_name,
           into = c("family", "individual", "task"),
           sep = "-") %>%
  select(family, individual,
         task, segment,
         eda_perc_est, eda_perc_cut,
         everything())



############################# JOIN ########################################

## ---------------------- EDA PARENT DATA + EDA PARENT EDITS -------------


eda <- right_join(parent_eda, parent_eda_edits,
                  by = c("family", "individual", "task", "segment")) %>%
  mutate_all(as.numeric)



######################### ADD VARIABLES PER TASK ###########################


eda <- eda %>%
  group_by(family, individual, task) %>%
  mutate(scl_mean_task = mean(tonic_scl)) %>%
  select(family, individual, task, segment,
         tonic_scl, scl_mean_task,
         total_scr,
         everything())


######################## EXPORT EDITED DF TO XLS ############################

## ---------------- export parent eda data to xls ------------------------------

write_xlsx(eda, "./output/parent_eda.xlsx")

