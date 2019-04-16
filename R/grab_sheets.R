path_name <- "tests/testthat/test_data/eda"
extension <- ".xlsx"

list_of_sheets <- workbook_sheets(path_name, extension)

sheet_name <- "EDA_Stats"

subset_for_sheets <- function(list_of_sheets, sheet_name){
  my_dfs <- grep(pattern = sheet_name, x = names(list_of_sheets))

  for(i in my_dfs){
    assign(names(list_of_sheets)[i], list_of_sheets[[i]], envir=globalenv())
  }

}

subset_for_sheets(list_of_sheets, sheet_name)
