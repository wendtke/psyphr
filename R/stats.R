#' Process "* Stats" sheet
#'
#' The transform_stats_sheet function transforms the raw HRV or EDA Stats sheet
#' (sheet #1) to use the Segment Names as column names, make all values numeric,
#' and add the name of the file as a column, so the data includes the
#' source designation.
#'
#' @param file_name
#' @param sheet
#'
#' @return a dataframe of segment data with a column containing file name
#' @export
#'
#' @examples
#' transform_stats_sheet(name_of_file, sheet_data)
#'
transform_stats_sheet <- function(file_name, sheet){
  sheet <- sheet %>%
    tidyr::gather(-`Segment Number`,
                  key = "segment",
                  value = "value") %>%
    tidyr::spread(key = `Segment Number`,
                  value = value) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(file_name = file_name)

  sheet

}
