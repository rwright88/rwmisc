# TODO
# add tests for strw_count()

# from rlang::is_installed()
is_installed <- function(pkg) {
  identical(requireNamespace(pkg, quietly = TRUE), TRUE)
}

strw_count <- function(string, pattern, perl = FALSE, fixed = FALSE) {
  out <- nchar(string) - nchar(gsub(pattern, "", string, perl = perl, fixed = fixed))
  out / nchar(pattern)
}
