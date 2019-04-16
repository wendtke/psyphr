#Make a list of dataframes from workbook sheets

path_name <- "tests/testthat/test_data/eda"
extension <- ".xlsx"

list_of_sheets <- workbook_sheets(path_name, extension)

#Subset the list of dataframes for a particular sheet name, eg "EDA_Stats"

sheet_name <- "EDA_Stats"
subset_for_sheets(list_of_sheets = list_of_sheets, sheet_name = sheet_name)

#Subset for list of dataframes for a particular file name, eg. "01-1-1"

file_name <- "01-1-1"
subset_for_files(list_of_sheets = list_of_sheets, file_name = file_name)

