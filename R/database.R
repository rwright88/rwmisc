# TODO
# add options for reading delimited file (skip, etc), database index?
# add tests

#' Write data files to database table
#'
#' Write delimited data files to database table in batches. Data files must have
#' the same headers. If the database file already exists, it will be deleted
#' first.
#' @param files Paths of delimited data files
#' @param file_db Path of database
#' @param table_name Database table name
#' @param batch_size Number of data files to write to database table per batch
#' @export
db_write_files <- function(files, file_db, table_name, batch_size = 1) {
  if (!(is_installed("data.table") && is_installed("DBI") && is_installed("RSQLite"))) {
    stop("`db_write_files` requires data.table, DBI, and RSQLite.", call. = FALSE)
  }

  n_files <- length(files)
  if (length(files) < 1) {
    stop("Length of `files` must be at least 1.", call. = FALSE)
  }
  if (!(is.numeric(batch_size) && batch_size >= 1 && batch_size <= n_files)) {
    stop("`batch_size` must be between 1 and the number of `files`.")
  }

  files_exist <- all(vapply(files, FUN.VALUE = logical(1), FUN = file.exists))
  if (files_exist != TRUE) {
    stop("One or more of the files do not exist.", call. = FALSE)
  }

  if (n_files > 1) {
    headers <- lapply(files, function(.x) {
      sort(names(data.table::fread(.x, nrows = 0, header = TRUE)))
    })
    if (identical_all(headers) != TRUE) {
      stop("Files have inconsistent headers.", call. = FALSE)
    }
  }

  if (file.exists(file_db)) {
    file.remove(file_db)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))

  batch_size <- round(batch_size)
  n_batches <- ceiling(n_files / batch_size)
  var_order <- names(data.table::fread(files[1], nrows = 0, header = TRUE))

  for (i in seq_len(n_batches)) {
    first <- (i - 1) * batch_size + 1
    last <- min(first + batch_size - 1, n_files)
    files_batch <- files[first:last]

    data <- lapply(files_batch, function(.x) {
      data <- data.table::fread(.x, header = TRUE)
      data.table::setcolorder(data, neworder = var_order)
      data
    })

    data <- data.table::rbindlist(data)

    DBI::dbWriteTable(
      conn = con,
      name = table_name,
      value = data,
      overwrite = FALSE,
      append = TRUE
    )
  }
}
