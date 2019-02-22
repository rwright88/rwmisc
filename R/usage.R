# TODO
# pkg_count_calls() is unfinished

#' Count function calls of package imports from source
#'
#' @param dir Directory of package
#' @return Data frame of rows as files in directory `R` and columns as function
#'   calls from a package that is imported.
#' @export
pkg_count_calls <- function(dir) {
  file_desc <- file.path(dir, "DESCRIPTION")
  dir_r <- file.path(dir, "R")
  stopifnot(file.exists(file_desc), file.exists(dir_r))

  desc <- read.dcf(file_desc)

  if (!("Imports" %in% colnames(desc))) {
    return(print("Package has no imports."))
  } else {
    imports <- desc[, "Imports"]
    imports <- strsplit(imports, split = ",")[[1]]
    imports <- stringr::str_remove_all(imports, "\\n")
    imports <- stringr::str_remove_all(imports, "\\(.*\\)")
    imports <- stringr::str_trim(imports)
    calls <- paste0(imports, "::")
  }

  files <- list.files(dir_r, "\\.[R]$", full.names = TRUE, ignore.case = TRUE)
  lines <- lapply(files, readLines)

  n_calls <- vector("list", length(calls))
  n_calls <- setNames(n_calls, calls)

  for (call in calls) {
    n_calls[[call]] <- lapply(lines, function(.x) {
      sum(stringr::str_count(.x, pattern = call))
    })
    n_calls[[call]] <- as.integer(n_calls[[call]])
  }

  n_calls <- dplyr::as_tibble(n_calls)
  n_calls$file <- basename(files)
  n_calls <- n_calls[c(length(n_calls), 1:(length(n_calls) - 1))]
  n_calls
}
