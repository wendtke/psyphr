context("Test the Settings sheet processing")

test_that("editing works for Settings from HRV workbook", {
  workbook_file_path <- "test_data/hrv/01_1_1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 9, col_names = FALSE)
  t_sheet <- transform_settings_sheet("01_1_1", sheet)
  exp_names <- c("50/60 hz Notch Filter",
                 "Baseline/Muscle Noise Filter",
                 "BioLab Filter",
                 "Calculation Method",
                 "Date",
                 "ECG Inverted",
                 "Edit File Name",
                 "Edit File Used?",
                 "End Time",
                 "File Name",
                 "HF/RSA Frequency Band",
                 "IBI Min/Max Check",
                 "Interval Period",
                 "LF Frequency Band",
                 "MAD MED Check",
                 "Maximum Heart Rate (BPM)",
                 "Minimum Heart Rate (BPM)",
                 "Mode",
                 "Respiration Max",
                 "Respiration Min",
                 "Respiration Signal Used",
                 "Sampling Frequency",
                 "Segment Time",
                 "Show Events",
                 "Start Time",
                 "Time",
                 "Use HF/RSA Band for Resp Rate",
                 "Version",
                 "VLF Frequency Band",
                 "Windowing",
                 "file_name")
  expect_equal(names(t_sheet), exp_names)
})

test_that("editing works for settings from EDA workbook", {
  workbook_file_path <- "test_data/eda/01-1-1.xlsx"
  sheet <- readxl::read_xlsx(workbook_file_path, sheet = 4, col_names = FALSE)
  t_sheet <- transform_settings_sheet("01-1-1", sheet)
  exp_names <- c("BioLab Filter",
                 "Block Size",
                 "Calculation Method",
                 "Date",
                 "Edit File Name",
                 "Edit File Used?",
                 "End Time",
                 "ER-SCR Latency Max",
                 "ER-SCR Latency Min",
                 "File Name",
                 "Interval Period",
                 "Mode",
                 "Override Gain Settings",
                 "Respiration Signal Used",
                 "Rolling Filter On/Off",
                 "Sampling Frequency",
                 "Segment Time",
                 "Show Events",
                 "Start Time",
                 "Time",
                 "uS Threshold",
                 "uS/Volt",
                 "Use All Events",
                 "Version",
                 "file_name")
  expect_equal(names(t_sheet), exp_names)
})
