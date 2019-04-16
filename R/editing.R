
#' Process "Editing Stats" sheet
#'
#' The transform_editing_sheet function transforms the raw Editing Stats sheet to use the Segment Names as
#' column names, make all values numeric, and add the name of the file as a column, so the data includes
#' the source designation.
#'
#' The rename_hrv-editing_columns function changes the column names to be easier to use by later analysis scripts.
#' It should only be called after transform_editing_sheet has already been used.
#'
#' @param file_name
#' @param sheet
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' transform_editing_sheet(name_of_file, sheet_data)
#' rename_hrv_editing_columns(sheet_data)
#'
transform_editing_sheet <- function(file_name, sheet){
  sheet <- sheet %>%
    tidyr::gather(-`Segment Number`,
           key = "segment",
           value = "value") %>%
    tidyr::spread(key = `Segment Number`,
           value = value) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(file_name = file_name)
    # mutate(file_name = sub(".xlsx", "", basename(file_name)))

  sheet

}

# TODO: add error handling in case this is run before transform_editing_sheet has been run, and the
# column names are not as expected. I.e., detect whether the second calumn name is a number, and if so,
# exit out gracefully with a message to run transform_editing_sheet first.
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


