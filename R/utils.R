#' Mindware Sample Study
#'
#' @return path to sample MindWare study; character string
#' @export
#'
sample_study_MW <- function(){
  system.file("extdata/MW/study", package = "psyphr")
}


#' Recursively Print out the Structure of a List
#'
#' @param x a list, or another subsettable object
#' @param unclass expand list-like objects
#' @param compact don't show unnamed elements
#' @param rich show length & memory size of element
#' @param all.names show hidden objects in an environment
#'
#' @return character string of the screen output
#' @export
#'
tree <- function(x = .GlobalEnv, unclass = FALSE, compact = TRUE, rich = FALSE, all.names = FALSE){
  if (!(mode(x) %in% c("list", "environment") & length(x) > 0)){
    stop("Please provide a non-empty list or environment")
  }

  element_profile <- function(name, body){
    c(
      "$", name,
      " (",paste(class(body), collapse = ", "), ") ", # class(es)
      `if`(rich,c(
        "|",
        ifelse(is.null(dim(body)), # length or dim
               length(body),
               paste(dim(body),
                     collapse = " by ")),
        ", ",
        format(object.size(body), # size (in memory)
               units = "auto",
               standard = "SI"),
        "|"
      ))
    )
  }

  traverse <- function(x, level = 1, branches_end = c()){
    element_names <- # vec of names at current level
      if (is.list(x)) {
        if (!is.null(names(x))){
          names(x)
        } else {
          rep("", length(x))
        }
      } else if (is.environment(x)){
        objects(x, all.names = all.names)
      }

    for (i in 1:length(element_names)){
      branches_end[level] <- i == length(element_names) # logical vec, branch ended in all levels?
      element <- if (is.list(x)){ # element itself
        x[[i]]
      } else if (is.environment(x)){
        get(element_names[i], x)
      }

      indent <- if (level == 1) {
        NULL
      } else {
        sapply(branches_end[1:(level - 1)],
               function(x) `if`(x, "    ", "│    "))
      }

      # print line
      if (compact & is.null(names(x))){ # compact when no-name
        NULL
      } else {
        cat(
          # rep("    ", level - 1),
          indent,
          `if`(branches_end[level], "└── ", "├── "), # branch
          element_profile(name = element_names[i],
                          body = element),
          "\n",
          sep = ""
        )
      }

      # Next level?
      element_class <- `if`(unclass,
                            mode(element),
                            class(element)
      )
      if (any(element_class %in% c("list", "environment")) & length(element) > 0){
        traverse(x = element, level = level + 1, branches_end = branches_end)
      }
    }
  }

  catn <- function(...){
    cat(..., sep = "\n")
  }

  out <- capture.output({
    cat(element_profile(name = deparse(substitute(x)),
                        body = x),"\n", sep = "")
    traverse(x)
  })

  catn(out)
  invisible(out)
}
