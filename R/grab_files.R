#See example for how this works

subset_for_files <- function(list_of_sheets, file_name){
  my_dfs <- grep(pattern = file_name, x = names(list_of_sheets))

  for(i in my_dfs){
    assign(names(list_of_sheets)[i], list_of_sheets[[i]], envir=globalenv())
  }

}
