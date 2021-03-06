# TODO
# add tests

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

  if ("Imports" %in% colnames(desc)) {
    imports <- desc[, "Imports"]
    imports <- strsplit(imports, split = ",")[[1]]
    imports <- gsub("\\n", "", imports, perl = TRUE)
    imports <- gsub("\\(.*\\)", "", imports, perl = TRUE)
    calls <- paste0(imports, "::")
  } else {
    return(print("Package has no imports."))
  }

  files <- list.files(dir_r, "\\.[R]$", full.names = TRUE, ignore.case = TRUE)
  lines <- lapply(files, readLines)

  out <- vector("list", length(calls))
  out <- set_names(out, calls)

  for (call in calls) {
    out[[call]] <- vapply(lines, FUN.VALUE = 0, FUN = function(.x) {
      sum(strw_count(.x, pattern = call, fixed = TRUE))
    })
  }

  out <- data.table::as.data.table(out)
  ord <- c("file", names(out))
  out$file <- basename(files)
  data.table::setcolorder(out, ord)
  out
}
