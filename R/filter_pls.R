# PLS methods

filter_imp_pls <-
  new_filter_method(
    name = "imp_pls",
    label = "Partial Least Squares Variable Importance",
    goal = "maximize",
    inputs = "quantitative",
    outputs = "all",
    pkgs = "mixOmics"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_imp_pls <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  validate_filter_data(object, x, y)

  p <- ncol(x)

  cl_fit <- rlang::call2("pls", .ns = "mixOmics", X = quote(x), Y = quote(y),
                         mode = "classic", ...)
  fit <- try(rlang::eval_tidy(cl_fit), silent = TRUE)

  if (inherits(fit, "try-error")) {
    res <- rep(NA_real_, p)
    names(res) <- names(x)
  } else {
    cl_imp <- rlang::call2("vip", .ns = "mixOmics", object = quote(fit))
    res <- try(rlang::eval_tidy(cl_imp), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- rep(NA_real_, p)
      names(res) <- names(x)
    } else {
      # Weight the components by the percent of total variance that
      # each accounts for.
      pct_y <- fit$prop_expl_var$Y / sum(fit$prop_expl_var$Y)
      res <- res %*% pct_y
      nms <- rownames(res)
      res <- res[,1]
      names(res) <- nms
    }
  }

  res <-
    new_filter_results(
      names(res),
      unname(res),
      object,
      rename = rename,
      num_pred = p
    )
  res
}
