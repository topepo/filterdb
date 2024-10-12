# ------------------------------------------------------------------------------
# Correlation filters

filter_corr <-
  new_filter_method(
    name = "corr",
    label = "Correlation Filter",
    goal = "maximize_abs",
    inputs = c("double", "integer"),
    outputs = c("double", "integer")
  )

# TODO don't need, p, goal
# TODO add a list of data hooks for additional checks

#' Execute a single supervised filter
#'
#' @param object An object of class `filter_method`.
#' @param x A data frame of predictors.
#' @param y A data frame with the outcome column.
#' @param seed A single integer to set the seed. If `NULL`, the current RNG
#' state is used.
#' @param ... Options passed to the underlying method (if any).
#' @return A tibble with a character column (`variable`) and a column for the
#' score values.
#' @export
fit_xy.filter_method_corr <- function(object, x, y,  ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  p <- ncol(x)

  res <- cor(dplyr::bind_cols(y, x), use = "pairwise.complete.obs", ...)
  res <- abs(res[1, -1])

  score <- new_score_vec(unname(res), direction = "maximize_abs", impute = 1.0)

  res <- new_filter_results(names(x), score, object, num_pred = p)
  res
}

###

filter_corr_rank <-
  new_filter_method(
    name = "corr_rank",
    label = "Rank Correlation Filter",
    goal = "maximize_abs",
    inputs = c("double", "integer"),
    outputs = c("double", "integer")
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_corr_rank <- function(object, x, y,  ...) {
  fit_xy.filter_method_corr(object, x, y, method = "spearman")
}
