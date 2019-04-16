
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
