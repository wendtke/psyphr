library("readxl")

path_name <- "tests/testthat/test_data/hrv"

# Find files in directory

files_full_path <- list.files(path = path_name, pattern = ".xlsx", full.names = T)
file_names <- list.files(path = path_name, pattern = ".xlsx", full.names = FALSE)
file_names <- gsub(pattern = ".xlsx", replacement = "", x = file_names)

#Make names for dataframes

df_names <- list()
for(i in 1:length(files_full_path)){
  sheet_names <- excel_sheets(files_full_path[i])
  df_names[[i]] <- paste(file_names[i], sheet_names, sep = "_")
  df_names[[i]] <- gsub(pattern = " ", replacement = "_", x = df_names[[i]])
}

# Read files

list_of_sheets <- list()
for(i in 1:length(files_full_path)){
  sheet_names <- excel_sheets(files_full_path[i])

  for(j in 1:length(sheet_names)){
    list_of_sheets[[j]] <- read_excel(files_full_path[i], sheet = sheet_names[j])
    assign(df_names[[i]][j], list_of_sheets[[j]])
  }
}
