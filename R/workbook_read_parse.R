#' Read a MindWare EDA Workbook
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
read_MW_EDA <- function(path) {
  read_MW_workbook(path, device_vendor = "MindWare") %>%
    tidy_MW_EDA()
}


#' Read a MindWare Workbook in Excel format
#'
#' @param path
#' @param device_vendor
#'
#' @return
#' @export
#'
#'
#' @examples
read_MW_workbook <- function(path, device_vendor = NULL){
  # Check if file type is Excel
  `if`(is.na(readxl::excel_format(path)), stop("The input is not an Excel file"))

  sheet_names <- readxl::excel_sheets(path)

  # Read each sheet from workbook
  suppressMessages({
  workbook <- purrr::map(sheet_names,
                  ~ readxl::read_excel(path = path,
                               sheet = .,
                               na = c("", "N/A"),
                               col_names = FALSE,
                               col_types = "text")
  ) %>% magrittr::set_names(sheet_names)

  })

  structure(
    workbook,
    class = c("psyphr_wb", class(workbook)),
    device_vendor = device_vendor,
    origin_path = path,
    origin_mtime = file.mtime(path)
    )
}



#' Parsing and tidying a Mindware EDA workbook
#'
#' @param workbook
#'
#' @return
#' @export
#'
#' @examples
tidy_MW_EDA <- function(workbook){
  # EDA Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # SCR Stats
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Editing Stats
  workbook[[3]] <- workbook[[3]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[4]] <- workbook[[4]] %>%
    df_to_vector()

  return(workbook)
}

tidy_MW_HRV <- function(workbook){
  # HRV Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # IBI
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Power Band Stats
  workbook[[3]] <- workbook[[3]] %>%
    transpose_convert_colnames()

  # Heart Rate Time Series
  workbook[[4]] <- workbook[[4]] %>%
    first_row_to_colnames()

  # Heart Period Power Spectrum
  hr_delta_f <- workbook[[5]][1,2]
  workbook[[5]] <- workbook[[5]][2:nrow(workbook[[5]]), ] %>%
    first_row_to_colnames()
  attributes(workbook[[5]], "HR Delta F") <- hr_delta_f


  # Respiration Time Series
  workbook[[6]] <- workbook[[6]] %>%
    first_row_to_colnames()

  # Respiration Power Spectrum
  resp_delta <- workbook[[7]][1,2]
  workbook[[7]] <- workbook[[7]][2:nrow(workbook[[7]]), ] %>%
    first_row_to_colnames()
  attributes(workbook[[7]], "Resp Delta") <- resp_delta

  # Editing Stats
  workbook[[8]] <- workbook[[8]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[9]] <- workbook[[9]] %>%
    df_to_vector()
}


#' Turn a data frame into vector
#'
#' Use a data frame's first column to a vectors's names, the second column to its values.
#'
#' @param .data
#'
#' @return
#'
df_to_vector <- function(.data){
  res <- .data[[2]]
  names(res) <- .data[[1]]
  res
}

transpose_convert_colnames <- function(.data) {
  .data %>%
    t() %>%
    first_row_to_colnames() %>%
    tibble::as_tibble()
}

first_row_to_colnames <- function(.data){
  colnames(.data) <- .data[1,]
  .data[-1,]
}


#' Bare Name of a File, w.o. Path or Extension
#'
#' @param path
#'
#' @return string
#' @export
#'
#' @examples
bare_name <- function(path){
  gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
}
