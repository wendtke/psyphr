context("Test the HRV or EDA Stats sheet processing")

test_that("editing works for Stats from HRV workwork", {
  workbook_file_path <- "test_data/hrv/01_1_1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 1)
  t_sheet <- transform_stats_sheet("01_1_1", sheet)
  exp_names <- c( "segment",
                  "# of R's Found",
                  "AVNN",
                  "End Event",
                  "End Time",
                  "First ECG R Time",
                  "First R to Last R Duration",
                  "Last ECG R Time",
                  "Manual Override",
                  "Mean Heart Rate",
                  "Mean IBI",
                  "NN50",
                  "pNN50",
                  "Respiration Amplitude",
                  "Respiration Peak Frequency",
                  "Respiration Power",
                  "Respiration Rate",
                  "RMSSD",
                  "RSA",
                  "SDNN",
                  "Segment Duration",
                  "Start Event",
                  "Start Time",
                  "Threshold (volts)",
                  "file_name")
  expect_equal(names(t_sheet), exp_names)

})


test_that("editing works for Stats from EDA workbook", {
  workbook_file_path <- "test_data/eda/01-1-1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 1)
  t_sheet <- transform_stats_sheet("01-1-1", sheet)
  exp_names <- c("segment",
                 "End Event",
                 "End Time",
                 "ER-SCRs",
                 "Mean SC",
                 "NS-SCRs",
                 "Segment Duration",
                 "Start Event",
                 "Start Time",
                 "Tonic Period",
                 "Tonic SCL",
                 "Total SCRs",
                 "file_name")
  expect_equal(names(t_sheet), exp_names)
})
