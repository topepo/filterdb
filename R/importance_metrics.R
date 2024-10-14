#' Compute univariate scores of predictors
#'
#' @param methods A character string of available scores. See [filter_methods()].
#' @param x A data frame of predictors.
#' @param y A data frame with a single outcome column.
#' @param A named list of options for the chosen `methods`.
#' @return An object with class `importance_metrics`. The elements are `values`
#' (a list of data frames of scores) and `info` (a list with the details of each
#' method).
#' @examples
#' if (rlang::is_installed(c("dplyr", "modeldata"))) {
#'   library(dplyr)
#'   library(modeldata)
#'
#'   ames_scores <-
#'     importance_metrics(
#'       ames %>% select(-Sale_Price),
#'       y = ames %>% select(Sale_Price),
#'       methods = c("corr")
#'     )
#'
#'   ames_scores
#' }
#'
#' @export
importance_metrics <- function(x, y, methods, opts = list()) {
  cl <- rlang::caller_env()

  all_known_methods <- filter_methods()
  methods <- rlang::arg_match(methods, all_known_methods, multiple = TRUE)

  # extract filter objects
  filter_info <- get_filter_info(methods)

  # get data requirements
  filter_info <- check_data_for_filters(filter_info, x, y, call = cl)

  # check any options
  check_user_options <- NULL

  # map over filters to get estimates
  res <- purrr::map(filter_info, ~ fit_xy(.x, x, y))
  names(res) <- purrr::map_chr(filter_info, ~ .x$name)

  res <- list(values = res, info = filter_info)
  class(res) <- c("importance_metrics")
  res

}

# ------------------------------------------------------------------------------
# Methods

#' @export
tidy.importance_metrics <- function(x, impute = TRUE, transform = TRUE, ...) {
  nms <- purrr::map_chr(x$info, ~ .x$name)

  vals <- purrr::map2(x$values, nms, ~ rlang::set_names(.x, c("term", .y)))
  num_scores <- length(vals)

  score_df <- vals[[1]]
  if (num_scores > 1) {
    for (i in 2:num_scores) {
      score_df <- dplyr::full_join(score_df, vals[[i]], by = "term")
    }
  }

  if (impute) {
    for (i in nms) {
      score_df[[i]] <- impute_score(score_df[[i]])
    }
  }
  if (transform) {
    for (i in nms) {
      score_df[[i]] <- transform(score_df[[i]])
    }
  }
  score_df
}


#' @export
print.importance_metrics <- function(x, ...) {
  cli::cli_inform("Univariate importance scores")
  labs <- purrr::map_chr(x$info, ~ .x$label)
  num_scores <- purrr::map_int(x$values, nrow)
  metric_list <- purrr::map2_chr(labs, num_scores,
                                 ~ cli::format_inline("{.x}: {.y} predictor{?s}"))
  names(metric_list) <- rep("*", length(metric_list))
  cli::cli_bullets(metric_list)
  invisible(x)
}

# ------------------------------------------------------------------------------
# Filter-related functions

get_filter <- function(x, call) {
  y <- paste0("filter_", x)
  res <- try(utils::getFromNamespace(y, "filterdb"), silent = TRUE)
  if (inherits(res, "try-error")) {
    msg <- as.character(res)
    cli::cli_warn("No information was found for filter {.val {x}}. Skipping...",
                  call = call)
    res <- character(0)
  }
  res
}

get_filter_info <- function(x, call = rlang::caller_env()) {
  res <- purrr::map(x, ~ get_filter(.x, call))
  has_info <- purrr::map_lgl(res, ~ !identical(.x, character(0)))
  res[has_info]
}
