#' Test objects in list for exact equality
#'
#' @param x List of objects
#' @param ... Additional arguments passed on to `identical()`
#' @return A single logical value.
#' @export
identical_all <- function(x, ...) {
  if (!is.list(x) || length(x) < 2) {
    stop("`x` must be a list of at least length 2.", call. = FALSE)
  }

  all(vapply(x[-1], FUN.VALUE = logical(1), FUN = function(.x) {
    identical(.x, x[[1]], ...)
  }))
}

# from rlang::is_installed()
is_installed <- function(pkg) {
  identical(requireNamespace(pkg, quietly = TRUE), TRUE)
}

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
  out <- stats::setNames(out, calls)

  for (call in calls) {
    out[[call]] <- vapply(lines, FUN.VALUE = integer(1), FUN = function(.x) {
      sum(stringr::str_count(.x, pattern = call))
    })
  }

  out <- dplyr::bind_rows(out)
  ord <- c("file", names(out))
  out$file <- basename(files)
  out[, ord]
}
