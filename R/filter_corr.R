# ------------------------------------------------------------------------------
# Correlation filters

# nocov start
filter_corr <-
  new_filter_method(
    name = "corr",
    label = "Correlation Filter",
    goal = "maximize",
    inputs = "quantitative",
    outputs = "quantitative"
  )
# nocov end

#' Execute a supervised filter
#'
#' @param object An object of class `filter_method`.
#' @param x A data frame of predictors.
#' @param y A data frame with the outcome column.
#' @param rename A logical; if `TRUE` then the results are given the name of the
#' filter (e.g., `corr`). Otherwise the column is named `score`.
#' @param seed A single integer to set the seed. If `NULL`, the current RNG
#' state is used.
#' @param ... Options passed to the underlying method (if any).
#' @return A tibble with a character column (`variable`) and a column for the
#' score values.
#' @export
fit_xy.filter_method_corr <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  validate_filter_data(object, x, y)

  p <- ncol(x)

  res <- cor(dplyr::bind_cols(y, x), use = "pairwise.complete.obs", ...)
  res <- abs(res[1, -1])

  res <- new_filter_results(names(x), res, object, rename = rename, num_pred = p)
  res
}

###

# nocov start
filter_corr_rank <-
  new_filter_method(
    name = "corr_rank",
    label = "Rank Correlation Filter",
    goal = "maximize",
    inputs = "quantitative",
    outputs = "quantitative"
  )
# nocov end

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_corr_rank <- function(object, x, y, rename = FALSE, ...) {
  fit_xy.filter_method_corr(object, x, y,  rename = rename, method = "spearman")
}
