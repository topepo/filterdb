# ------------------------------------------------------------------------------
# Variable importance

filter_imp_rf <-
  new_filter_method(
    name = "imp_rf",
    label = "Random Forest Variable Importance",
    predictor_types = c("double", "integer", "factor"),
    outcome_types = c("double", "integer", "factor"),
    pkgs = "ranger"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_imp_rf <- function(object, x, y,
                                        seed = sample.int(1000, 1), ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  y <- y[[1]]
  p <- ncol(x)

  # check dots for 'importance'; when not set or use 'none'
  cl <- rlang::call2("ranger", .ns = "ranger", x = quote(x), y = quote(y),
                     importance = "impurity_corrected", ...)
  set.seed(seed) # TODO use withr::with_seed?
  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- list(variable.importance = rep(NA_real_, p))
    names(res$variable.importance) <- names(x)
  }

  scores <- score_vec(unname(res$variable.importance), impute = Inf)

  res <-
    new_filter_results(
      names(res$variable.importance),
      scores,
      object,
     
    )
  res
}

###


filter_imp_crf <-
  new_filter_method(
    name = "imp_crf",
    label = "Random Forest Variable Importance (Conditional Inference)",
    predictor_types = c("double", "integer", "factor"),
    outcome_types = c("double", "integer", "factor"),
    pkgs = "party"
  )

#' @rdname fit_xy.filter_method_corr
#' @export
fit_xy.filter_method_imp_crf <- function(object, x, y,
                                         seed = sample.int(1000, 1), ...) {
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(object, x, y)
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]

  dat <- dplyr::bind_cols(x, y)
  f <- paste0(names(y), "~.")
  f <- as.formula(f)

  y <- y[[1]]
  p <- ncol(x)

  cl_fit <- rlang::call2("cforest", .ns = "party", formula = quote(f), data = quote(dat), ...)
  set.seed(seed) # TODO use withr::with_seed?
  fit <- try(rlang::eval_tidy(cl_fit), silent = TRUE)

  if (inherits(fit, "try-error")) {
    res <- rep(NA_real_, p)
    names(res) <- names(x)
  } else {
    cl_imp <- rlang::call2("varimp", .ns = "party", object = quote(fit))
    res <- try(rlang::eval_tidy(cl_imp), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- rep(NA_real_, p)
      names(res) <- names(x)
    }
  }

  scores <- score_vec(unname(res), impute = Inf)

  res <-
    new_filter_results(
      names(res),
      scores,
      object,
     
    )
  res
}
