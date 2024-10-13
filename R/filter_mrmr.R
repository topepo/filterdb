# ------------------------------------------------------------------------------
#

#' Minimum Redundancy Maximal Relevancy filter
#' @export
filter_mrmr <-
  new_filter_method(
    name = "mrmr",
    label = "Minimum Redundancy Maximal Relevancy Filter",
    predictor_types = c("numeric", "double", "integer", "factor"),
    outcome_types = "factor",
    pkgs = "praznik",
    case_weights = FALSE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_mrmr <- function(object, x, y, ...) {
  x <- dplyr::as_tibble(x)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  # TODO convert ints in 'x' to doubles. See ?praznik::MRMR

  y <- y[[1]]
  p <- ncol(x)

  cl <-
    rlang::call2(
      "MRMR", .ns = "praznik",
      X = quote(x), Y = quote(y),
      k = p, ...
    )
  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res$score <- rep(NA_real_, p)
    names(res) <- names(x)
  }
  predictors <- names(res$score)
  res$score <- new_score_vec(unname(res$score), direction = "maximize", impute = Inf)

  res <- new_filter_results(predictors, res$score , object)
  res
}
