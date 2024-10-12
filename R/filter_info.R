# ------------------------------------------------------------------------------
# Information


filter_info_gain <-
  new_filter_method(
    name = "info_gain",
    label = "Information Gain",
    goal = "maximize",
    inputs = c("double", "integer", "factor"),
    outputs = "factor",
    pkgs = "FSelectorRcpp"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_info_gain <- function(object, x, y, rename = FALSE, ...) {
  x <- as.data.frame(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  y <- y[[1]]
  p <- ncol(x)

  cl <- rlang::call2("information_gain", .ns = "FSelectorRcpp",
                     x = quote(x), y = quote(y), type = "infogain", ...)

  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- dplyr::tibble(variable = names(x), score = rep(NA_real_, p))
  } else {
    res <- setNames(res, c("variable", "score"))
  }

  score <- new_score_vec(res$score, direction = "maximize", impute = Inf)

  res <-
    new_filter_results(
      res$variable,
      score,
      object,
      rename = rename,
      num_pred = p
    )
  res
}

###

filter_info_gain_ratio <-
  new_filter_method(
    name = "info_gain_ratio",
    label = "Information Gain Ratio",
    goal = "maximize",
    inputs = c("double", "integer", "factor"),
    outputs = "factor",
    pkgs = "FSelectorRcpp"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_info_gain_ratio <- function(object, x, y, rename = FALSE, ...) {
  x <- as.data.frame(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  y <- y[[1]]
  p <- ncol(x)

  cl <- rlang::call2("information_gain", .ns = "FSelectorRcpp",
                     x = quote(x), y = quote(y), type = "gainratio", ...)

  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- dplyr::tibble(variable = names(x), score = rep(NA_real_, p))
  } else {
    res <- setNames(res, c("variable", "score"))
  }

  score <- new_score_vec(res$score, direction = "maximize", impute = Inf)

  res <-
    new_filter_results(
      res$variable,
      score,
      object,
      rename = rename,
      num_pred = p
    )
  res
}

###

filter_mic <-
  new_filter_method(
    name = "mic",
    label = "Maximal Information Coefficient",
    goal = "maximize",
    inputs = c("double", "integer"),
    outputs = c("double", "integer"),
    pkgs = "minerva"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_mic <- function(object, x, y, rename = FALSE, ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  x <- as.matrix(x)
  y <- y[[1]]
  p <- ncol(x)

  cl <- rlang::call2("mine", .ns = "minerva",
                     x = quote(x), y = quote(y),
                     use = "pairwise.complete.obs", ...)

  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- rep(NA_real_, p)
  } else {
    res <- res$MIC[,1]
  }

  score <- new_score_vec(res, direction = "maximize", impute = Inf)

  res <-
    new_filter_results(
      colnames(x),
      score,
      object,
      rename = rename,
      num_pred = p
    )
  res
}

