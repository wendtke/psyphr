#' Read a study from a directory
#'
#' @param path a character string; path to a study directory
#'
#' @return a data frame; psyphr study S3 object
#' @export
read_study <- function(path){

  file_paths <- list.files(path = path, pattern = ".xlsx$", full.names = TRUE)
  file_ids <- file_paths %>% bare_name() %>% stringr::str_split(pattern = "_")

  assertthat::assert_that(length(unique(lapply(file_ids, length))) == 1,
                          msg = "All file names must follow identical schema.")


  study <- file_ids %>%
    dplyr::bind_cols() %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(
      ~ .x %>% stringr::str_replace("V", "id_")
      ) %>%
    dplyr::mutate(data = file_paths %>% purrr::quitely(purrr::map(read_MW)),
           format = data %>% purrr::map(~ attributes(.x)["format"]) %>% unlist()
           )

  structure(study, class = c("psyphr_study", class(study)))

}

#' Print a Summary of a Psyphr Study
#'
#' @param study a psyphr study object
#'
#' @return NULL
#' @export
#'
#' @examples
print.psyphr_study <- function(study){
  #


}


#' Lift Metadata from Workbooks in a Study
#'
#' @param study a psyphr study object
#'
#' @return a psyphr study S3 object
#' @export
#'
lift_meta <- function(study){
  study %>%
    dplyr::mutate(settings = .data$data %>% purrr::map("Settings"))
}



#' Flatten a Study with a Recursive File Structure
#'
#' @param origin a character string; origin path
#' @param dest a character string; destination path
#' @param delim a character string; file name delimiter
#'
#' @export
flatten_study_dir <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = TRUE)
  `if`(any(grepl(delim, origin_file_name)), stop("path should not contain delimiter"))
  dest_file_name <- gsub("/", delim, origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest, dest_file_name)

  for (i in seq_along(origin_file_path)) {
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}

#' Lift a Study with a Flat File Structure
#'
#' @param origin a character string; origin path
#' @param dest a character string; destination path
#' @param delim a character string; file name delimiter
#'
#' @export
lift_study_dir <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = FALSE)
  dest_file_name <- gsub(delim, "/", origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest,dest_file_name)

  for (i in seq_along(origin_file_path)){
    if (!dir.exists(dirname(dest_file_path[i]))) {
      dir.create(dirname(dest_file_path[i]), recursive = TRUE)
    }
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}

