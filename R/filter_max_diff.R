# ------------------------------------------------------------------------------
# Max difference in the outcome between groups of a factor predictor

# nocov start
filter_max_diff <-
  new_filter_method(
    name = "max_diff",
    label = "Maximum Group Difference",
    goal = "maximize",
    inputs = "qualitative",
    outputs = "all"
  )
# nocov end

comp_max_diff <- function(x, y) {
  dat <- data.frame(x = x, y = y)
  if (is.factor(y)) {
    fam <- "binomial"
  } else {
    fam <- "gaussian"
  }
  mod_fit <- try(stats::glm(y ~ x + 0, data = dat, family = fam), silent = TRUE)
  if (inherits(mod_fit, "try-error")) {
    res <- NA_real_
  } else {
    res <- coef(mod_fit)
    res <- max(res, na.rm = TRUE) - min(res, na.rm = TRUE)
  }
  res
}

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_max_diff <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  validate_filter_data(object, x, y)

  p <- ncol(x)

  res <- purrr::map_dbl(x, ~ comp_max_diff(.x, y = y[[1]]))
  res <- new_filter_results(names(x), res, object, rename = rename, num_pred = p)
  res
}
