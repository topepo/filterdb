#' Correlation filter
#' @export
filter_corr <-
  new_filter_method(
    name = "corr",
    label = "Correlation Filter",
    predictor_types = c("numeric", "double", "integer"),
    outcome_types = c("numeric", "double", "integer"),
    case_weights = FALSE
  )

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
#' @keywords internal
fit_xy.filter_method_corr <- function(object, x, y,  ...) {
  rlang::check_installed(object$pkgs)
 # TODO modularize
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  res <- cor(dplyr::bind_cols(y, x), use = "pairwise.complete.obs", ...)
  res <- res[1, -1]

  score <- new_score_vec(unname(res), direction = "maximize_abs", impute = 1.0)

  res <- new_filter_results(names(x), score, object)
  res
}

###

#' @rdname filter_corr
#' @export
filter_corr_rank <-
  new_filter_method(
    name = "corr_rank",
    label = "Rank Correlation Filter",
    predictor_types = c("numeric", "double", "integer"),
    outcome_types = c("numeric", "double", "integer"),
    case_weights = FALSE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_corr_rank <- function(object, x, y,  ...) {
  fit_xy.filter_method_corr(object, x, y, method = "spearman")
}
