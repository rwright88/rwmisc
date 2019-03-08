# TODO
# pkg_count_calls() is unfinished

#' Count function calls of package imports from package source
#'
#' @param dir Directory of package source
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

  out <- vector("list", length(calls))
  out <- setNames(out, calls)

  for (call in calls) {
    out[[call]] <- vapply(lines, FUN.VALUE = integer(1), FUN = function(.x) {
      sum(stringr::str_count(.x, pattern = call))
    })
  }

  ord <- c("file", names(out))
  data.table::setDT(out)
  out$file <- basename(files)
  out <- out[, ..ord]
  out
}
