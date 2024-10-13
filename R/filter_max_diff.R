#' ANOVA filters
#' @export
filter_max_diff <-
  new_filter_method(
    name = "max_diff",
    label = "Maximum Group Difference",
    predictor_types = "factor",
    outcome_types = c("numeric", "double", "integer", "factor"),
    case_weights = TRUE
  )

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
fit_xy.filter_method_max_diff <- function(object, x, y, ...) {
  rlang::check_installed(object$pkgs)
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  res <- purrr::map_dbl(x, ~ comp_max_diff(.x, y = y[[1]]))
  score <- new_score_vec(unname(res), direction = "maximize_abs", impute = Inf)
  res <- new_filter_results(names(x), score, object)
  res
}
