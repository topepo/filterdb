# ------------------------------------------------------------------------------
# Area under the ROC curve

# nocov start
filter_roc_auc <-
  new_filter_method(
    name = "roc_auc",
    label = "Area under the ROC Curve",
    goal = "maximize",
    inputs = "quantitative",
    outputs = "qualitative",
    pkgs = "pROC"
  )
# nocov end

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
fit_xy.filter_method_roc_auc <- function(object, x, y, rename = FALSE, ...) {
  # check empty dots
  # case weights? event_level?
  x <- dplyr::as_tibble(x)
  validate_filter_data(object, x, y)

  p <- ncol(x)

  # Add wrapper using call and catch errors
  roc_scores <- purrr::map_dbl(x, ~ roc_wrapper(x = .x, y = y[[1]])) # TODO add in the ...
  roc_scores <- ifelse(roc_scores < 0.5, 1 - roc_scores, roc_scores)

  res <- new_filter_results(names(x), roc_scores, object, rename = rename,
                            num_pred = p)
  res
}
