
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


