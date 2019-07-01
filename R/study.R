#' Flatten a Study with a Recursive File Structure
#'
#' @param origin origin path
#' @param dest destination path
#' @param delim file name delimiter
#'
#' @export
flatten_study <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = TRUE)
  dest_file_name <- gsub("/", delim, origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest, dest_file_name)

  for (i in seq_along(origin_file_path)) {
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}


