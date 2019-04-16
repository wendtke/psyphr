#See example for how this works

subset_for_sheets <- function(list_of_sheets, sheet_name){
  my_dfs <- grep(pattern = sheet_name, x = names(list_of_sheets))

  for(i in my_dfs){
    assign(names(list_of_sheets)[i], list_of_sheets[[i]], envir=globalenv())
  }

}
