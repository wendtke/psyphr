#' Rename sheet columns after the sheet has been transformed.
#'
#' Functions to change the column names to be easier to use by later analysis scripts.
#' They should only be called after transform_* has already been used.
#'
#' TODO: add error handling in case this is run before transform_* has been run,
#' and the column names are not as expected. I.e., detect whether the second
#' column name is a number, and if so,
#' exit out gracefully with a message to run transform_* function first.
#'
#' @param sheet
#'
#' @return a dataframe with renamed columns
#' @export
#'
#' @examples
#' rename_hrv_stats_columns(sheet_data)
#' rename_hrv_editing_columns(sheet_data)
#' rename_eda_stats_columns(sheet_data)
#' rename_hrv_editing_columns(sheet_data)
#'
#'
#'



rename_hrv_stats_columns <- function(sheet){
  sheet <- sheet %>%
    dplyr::rename(segment = segment,
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

  sheet

}



rename_hrv_editing_columns <- function(sheet) {
  sheet <- sheet %>%
    dplyr::rename(segment = segment,
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
                  est_r_r_perc = `% of Estimated R-R Intervals`)

  sheet

}


rename_eda_stats_columns <- function(sheet){
  sheet <- sheet %>%
    dpylr::rename(segment = segment,
                  start_time = `Start Time`,
                  end_time = `End Time`,
                  seg_length = `Segment Duration`,
                  total_scr = `Total SCRs`,
                  er_scr = `ER-SCRs`,
                  ns_scrs = `NS-SCRs`,
                  tonic_scl = `Tonic SCL`,
                  mean_sc = `Mean SC`,
                  tonic_period = `Tonic Period`)

  sheet

}



rename_eda_editing_columns <- function(sheet){
  sheet <- sheet %>%
    dplyr::rename(segment = segment,
                  eda_sec_cut = `EDA : Seconds Removed`,
                  eda_perc_cut = `EDA : Percentage Removed`,
                  eda_sec_est = `EDA : Seconds Estimated`,
                  eda_perc_est = `EDA : Percentage Estimated`,
                  resp_sec_cut = `Resp : Seconds Removed`,
                  resp_perc_cut = `Resp : Percentage Removed`,
                  resp_sec_est = `Resp : Seconds Estimated`,
                  resp_perc_est = `Resp : Percentage Estimated`)
}
