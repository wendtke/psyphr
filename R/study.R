#' Read a study from a directory
#'
#' @param path path to a study directory
#'
#' @return a nested data frame including IDs
#' @export
read_study <- function(path){

  file_paths <- list.files(path = path, pattern = ".xlsx$", full.names = TRUE)
  file_ids <- file_paths %>% bare_name() %>% stringr::str_split(file_bare_names, pattern = "_")

  assertthat::assert_that(length(unique(lapply(file_ids, length))) == 1,
                          msg = "All file names must follow identical schema.")


  study <- file_ids %>%
    dplyr::bind_cols() %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(
      ~ .x %>% str_replace("V", "id_")
      ) %>%
    dplyr::mutate(data = suppressWarnings(file_paths %>% purrr::map(read_MW)),
           format = data %>% purrr::map(~ attributes(.x)["format"]) %>% unlist()
           )

  structure(study, class = c("psyphr_study", class(study)))

}

#' Print a Summary of a Psyphr Study
#'
#' @param study
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
#' @param study a psyphr study
#'
#' @return a psyphr study
#' @export
#'
lift_meta <- function(study){
  study %>% dplyr::mutate(settings = .data$data %>% purrr::map("Settings"))
}



#' Flatten a Study with a Recursive File Structure
#'
#' @param origin origin path
#' @param dest destination path
#' @param delim file name delimiter
#'
#' @export
flatten_study_dir <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = TRUE)
  dest_file_name <- gsub("/", delim, origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest, dest_file_name)

  for (i in seq_along(origin_file_path)) {
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}


