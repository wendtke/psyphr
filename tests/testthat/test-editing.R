context("Test the Editing Stats sheet processing")

test_that("editing works for Editing Stats from HRV workbook", {
  workbook_file_path <- "test_data/hrv/01_1_1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 8)
  t_sheet <- transform_editing_sheet("01_1_1", sheet)
  exp_names <- c("segment",
                 "% Artifact Peaks",
                 "% Estimated Peaks",
                 "% Normal Peaks",
                 "% of Estimated R-R Intervals",
                 "Artifact Peaks",
                 "Duration of Estimated R-R Intervals",
                 "ECG : Percentage Estimated",
                 "ECG : Percentage Removed",
                 "ECG : Seconds Estimated",
                 "ECG : Seconds Removed",
                 "Estimated Peaks",
                 "Normal Peaks",
                 "Resp : Percentage Estimated",
                 "Resp : Percentage Removed",
                 "Resp : Seconds Estimated",
                 "Resp : Seconds Removed",
                 "Total Peaks",
                 "file_name")
  expect_equal(names(t_sheet), exp_names)
})

test_that("editing works for Editing Stats from EDA workbook", {
  workbook_file_path <- "test_data/eda/01-1-1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 3)
  t_sheet <- transform_editing_sheet("01-1-1", sheet)
  exp_names <- c("segment",
                 "EDA : Percentage Estimated",
                 "EDA : Percentage Removed",
                 "EDA : Seconds Estimated",
                 "EDA : Seconds Removed",
                 "Resp : Percentage Estimated",
                 "Resp : Percentage Removed",
                 "Resp : Seconds Estimated",
                 "Resp : Seconds Removed",
                 "file_name")
  expect_equal(names(t_sheet), exp_names)
})
