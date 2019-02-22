# TODO
# pac_count_calls() is unfinished

#' Count number of function calls
#'
#' @param dir Directory
#' @return Data frame
#' @export
pac_count_calls <- function(dir) {
  files <- list.files(dir, "\\.[R]$", full.names = TRUE, ignore.case = TRUE)
  lines <- lapply(files, read_lines)

  deps <- desc::desc_get_deps()
  deps <- deps[deps$type == "Imports", ]

  calls <- paste0(deps[["package"]], "::")

  if ("magrittr::" %in% calls) {
    calls <- c(calls, "%>%")
  }

  n_calls <- vector("list", length(calls))
  names(n_calls) <- calls

  for (call in calls) {
    n_calls[[call]] <- map_int(lines, ~sum(str_count(., call)))
  }

  n_calls <- as_tibble(n_calls) %>%
    mutate(file = files) %>%
    select(file, everything())

  n_calls
}
