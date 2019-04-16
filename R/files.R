library("readxl")

workbook_sheets <- function(pathname, extension){

  # Find files in directory

  files_full_path <- list.files(path = path_name, pattern = extension, full.names = T)
  file_names <- list.files(path = path_name, pattern = extension, full.names = FALSE)
  file_names <- gsub(pattern = extension, replacement = "", x = file_names)

  #Make names for dataframes

  df_names <- list()
  for(i in 1:length(files_full_path)){
    sheet_names <- excel_sheets(files_full_path[i])
    df_names[[i]] <- paste(file_names[i], sheet_names, sep = "_")
    df_names[[i]] <- gsub(pattern = " ", replacement = "_", x = df_names[[i]])
  }
  df_names <- unlist(df_names)

  # Read files

  list_of_wkbk_sheets <- list()
  list_item <- 1
  for(i in 1:length(files_full_path)){
    sheet_names <- excel_sheets(files_full_path[i])

    for(j in 1:length(sheet_names)){
      list_of_wkbk_sheets[[list_item]] <- as.data.frame(read_excel(files_full_path[i], sheet = sheet_names[j]))
      list_item <- list_item + 1
    }
  }

  names(list_of_wkbk_sheets) <- df_names

  list_of_wkbk_sheets

}
