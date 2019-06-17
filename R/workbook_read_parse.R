#' Read a MindWare Workbook
#'
#' @name read_MW
#' @rdname read_MW
#' @param path
#'
#' @return a list of Mindware data sheets
NULL

#' @rdname read_MW
#' @export
read_MW_EDA <- function(path) {
  read_MW_workbook(path, device_vendor = "MindWare") %>%
    tidy_MW_EDA()
}

#' @rdname read_MW_EDA
#' @export
read_MW_HRV <- function(path){
  read_MW_workbook(path, device_vendor = "MindWare") %>%
    tidy_MW_HRV()
}


#### Internal ####

# Read a MindWare Workbook in Excel format
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
    class = c("psyphr_workbook", class(workbook)),
    device_vendor = device_vendor,
    origin_path = path,
    origin_mtime = file.mtime(path)
    )
}


# Parsing and tidying a Mindware workbooks
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
  hr_delta_f <- workbook[[5]][1,1, drop = TRUE]
  workbook[[5]] <- workbook[[5]][2:nrow(workbook[[5]]), ] %>%
    first_row_to_colnames()
  attr(workbook[[5]], "HR Delta F") <- hr_delta_f


  # Respiration Time Series
  workbook[[6]] <- workbook[[6]] %>%
    first_row_to_colnames()

  # Respiration Power Spectrum
  resp_delta <- workbook[[7]][1,1, drop = TRUE]
  workbook[[7]] <- workbook[[7]][2:nrow(workbook[[7]]), ] %>%
    first_row_to_colnames()
  attr(workbook[[7]], "Resp Delta") <- resp_delta


  # Interval Stats
    # optional
  has_interval <- length(workbook) == 10 # if length == 10, no "interval" sheet

  if (has_interval){
    workbook[[7 + has_interval]] <- workbook[[7 + has_interval]] %>%
      first_row_to_colnames()
  }

  # Editing Stats
  workbook[[8 + has_interval]] <- workbook[[8 + has_interval]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[9 + has_interval]] <- workbook[[9 + has_interval]] %>%
    df_to_vector()

  return(workbook)
}



#### Parsing Helpers ####

# Turn a data frame into vector
# Data frame's first column as vectors' names, the second column as values
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
  .data[-1,,drop = FALSE]
}


# Bare Name of a File, w.o. Path or Extension
bare_name <- function(path){
  gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
}
