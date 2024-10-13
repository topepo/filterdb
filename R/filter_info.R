#' Information theory filters
#' @export
filter_info_gain <-
  new_filter_method(
    name = "info_gain",
    label = "Information Gain",
    predictor_types = c("numeric", "double", "integer", "factor"),
    outcome_types = "factor",
    pkgs = "FSelectorRcpp",
    case_weights = FALSE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_info_gain <- function(object, x, y, ...) {
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

    )
  res
}

###

#' @rdname filter_info_gain
#' @export
filter_info_gain_ratio <-
  new_filter_method(
    name = "info_gain_ratio",
    label = "Information Gain Ratio",
    predictor_types = c("numeric", "double", "integer", "factor"),
    outcome_types = "factor",
    pkgs = "FSelectorRcpp",
    case_weights = FALSE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_info_gain_ratio <- function(object, x, y, ...) {
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
  nan_score <- is.nan(res$score)
  if (any(nan_score)) {
    res$score[nan_score] <- NA_real_
  }

  score <- new_score_vec(res$score, direction = "maximize", impute = Inf)

  res <-
    new_filter_results(
      res$variable,
      score,
      object,

    )
  res
}

###

#' @rdname filter_info_gain
#' @export
filter_mic <-
  new_filter_method(
    name = "mic",
    label = "Maximal Information Coefficient",
    predictor_types = c("numeric", "double", "integer"),
    outcome_types = c("numeric", "double", "integer"),
    pkgs = "minerva",
    case_weights = FALSE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_mic <- function(object, x, y, ...) {
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
  res <- new_filter_results(colnames(x), score, object)
  res
}

