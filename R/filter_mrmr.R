# ------------------------------------------------------------------------------
# Minimum Redundancy Maximal Relevancy Filter

# nocov start
filter_mrmr <-
  new_filter_method(
    name = "mrmr",
    label = "Minimum Redundancy Maximal Relevancy Filter",
    goal = "maximize",
    inputs = "all",
    outputs = "qualitative",
    pkgs = "praznik"
  )
# nocov end

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_mrmr <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  validate_filter_data(object, x, y)
  # convert ints in 'x' to doubles. See ?praznik::MRMR
  x <- purrr::map(x, check_ints)
  x <- dplyr::bind_cols(x)
  is_complt <- vctrs:::vec_detect_complete(x) & vctrs:::vec_detect_complete(y)
  x <- x[is_complt,, drop = FALSE]
  y <- y[is_complt,, drop = FALSE]

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

  res <- new_filter_results(names(res$score), unname(res$score), object,
                          rename = rename, num_pred = p)
  res
}

