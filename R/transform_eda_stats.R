#' Process "EDA Stats" sheet
#'
#' The transform_eda_sheet function ...
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
transform_eda_sheet <- function(file_name, sheet, rename_columns = TRUE){
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
             total_scr = `Total SCRs`,
             er_scr = `ER-SCRs`,
             ns_scrs = `NS-SCRs`,
             tonic_scl = `Tonic SCL`,
             mean_sc = `Mean SC`,
             tonic_period = `Tonic Period`)
  }
}

