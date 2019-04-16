
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
