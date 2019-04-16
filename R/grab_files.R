path_name <- "tests/testthat/test_data/eda"
extension <- ".xlsx"

list_of_sheets <- workbook_sheets(path_name, extension)

file_name <- "01-1-1"

subset_for_files <- function(list_of_sheets, file_name){
  my_dfs <- grep(pattern = file_name, x = names(list_of_sheets))

  for(i in my_dfs){
    assign(names(list_of_sheets)[i], list_of_sheets[[i]], envir=globalenv())
  }

}

subset_for_files(list_of_sheets, file_name)

