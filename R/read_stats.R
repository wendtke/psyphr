#' Process main "Stats" sheet
#'
#' The transform_hrv_sheet function ...
#'
#' @param file_name
#' @param sheet
#' @param rename_columns
#'
#' @return
#' @export
#'
#' @examples
#'
transform_hrv_sheet <- function(file_name, sheet, rename_columns = TRUE){
  sheet_base <- sheet %>%
    gather(-`Segment Number`,
           key = "segment",
           value = "value") %>%
    spread(key = `Segment Number`,
           value = value) %>%
    mutate_all(as.numeric) %>%
    mutate(file_name = file_name)
  # mutate(file_name = sub(".xlsx", "", basename(file_name)))

  if(rename_columns){
    sheet_base <- sheet_base %>%
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
             pnn50 = pNN50)
  }
}
