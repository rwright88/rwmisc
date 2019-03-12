#' Calculate twitter score
#'
#' @param user Screen name or user ID
#' @return Double
#' @export
twt_user_score <- function(user) {
  followers <- rtweet::get_followers(user, n = 75000, retryonratelimit = TRUE)
  n_follows <- nrow(followers)
  limit <- 90000

  if (n_follows <= limit) {
    data <- rtweet::lookup_users(followers$user_id)
  } else {
    n_batches <- ceiling(n_follows / limit)
    data <- vector("list", length(n_batches))
    for (i in seq_along(n_batches)) {
      first <- (i - 1) * limit + 1
      last <- min(first + limit - 1, n_files)
      ids <- followers$user_id[first:last]
      data[[i]] <- rtweet::lookup_users(ids)
    }
    data <- data.table::rbindlist(data)
  }

  data <- data[data$friends_count > 0, ]
  out <- sum(data$followers_count / data$friends_count)
  out
}
