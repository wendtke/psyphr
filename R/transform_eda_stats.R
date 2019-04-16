#' Process "EDA Stats" sheet
#'
#' The transform_eda_sheet function transforms the raw EDA Stats sheet to use
#' the Segment column names, make all values numeric, and add the name of the
#' file as a column, so the data includes the source designation.
#'
#' @param file_name
#' @param sheet
#'
#' @return a dataframe of segment data with a column containing the file name
#' @export
#'
#' @examples
#' transform_eda_sheet(name_of_file, sheet_data)
#'
transform_eda_sheet <- function(file_name, sheet){
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
