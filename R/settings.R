#' Process "Settings" sheet
#'
#' The transform_settings_sheet function transforms the raw Settings sheet to "flip it on its side" so
#' variables are columns and values are rows
#'
#' IMPORTANT NOTE: This needs to be tested
#'
#' @param file_name
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
#'
#'
transform_settings_sheet <- function(file_name, sheet){
  sheet <- sheet %>%
    tidyr::gather(key = "setting",
                  value = "value") %>%
    dplyr::mutate(file_name = file_name)
}
