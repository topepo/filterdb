# ------------------------------------------------------------------------------
# Minimum Redundancy Maximal Relevancy Filter

filter_mrmr <-
  new_filter_method(
    name = "mrmr",
    label = "Minimum Redundancy Maximal Relevancy Filter",
    goal = "maximize", # TODO encode this in the score vector
    inputs = "all",
    outputs = "qualitative",
    pkgs = "praznik"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_mrmr <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  if (is.vector(y)) { # TODO do these outside of these functions
    y <- dplyr::as_tibble(y)
  }
  validate_filter_data(object, x, y)
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

  res <- new_filter_results(names(res$score), unname(res$score), object,
                          rename = rename, num_pred = p)
  res
}
