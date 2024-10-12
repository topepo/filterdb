# ------------------------------------------------------------------------------
# Area under the ROC curve

filter_roc_auc <-
  new_filter_method(
    name = "roc_auc",
    label = "Area under the ROC Curve",
    goal = "maximize",
    inputs = c("double", "integer"),
    outputs = "factor",
    pkgs = "pROC"
  )

roc_wrapper <- function(x, y, ...) {
  cl <-
    rlang::call2(
      "roc_auc_vec",
      .ns = "yardstick",
      truth = quote(y),
      estimate = quote(x),
      ...
    )
  res <- try(rlang::eval_tidy(cl))
  if (inherits(res, "try-error")) {
    res <- NA_real_
  }
  res
}

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_roc_auc <- function(object, x, y, ...) {
  # check empty dots
  # case weights? event_level?
  x <- dplyr::as_tibble(x)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  p <- ncol(x)

  # Add wrapper using call and catch errors
  roc_scores <- purrr::map_dbl(x, ~ roc_wrapper(x = .x, y = y[[1]])) # TODO add in the ...
  roc_scores <- ifelse(roc_scores < 0.5, 1 - roc_scores, roc_scores)
  score <- new_score_vec(unname(roc_scores), direction = "maximize", impute = 1.0)

  res <- new_filter_results(names(x), score, object,
                            num_pred = p)
  res
}
