#' Random forests importance filters
#' @export
filter_imp_rf <-
  new_filter_method(
    name = "imp_rf",
    label = "Random Forest Variable Importance",
    predictor_types = c("numeric", "double", "integer", "factor"),
    outcome_types = c("numeric", "double", "integer", "factor"),
    pkgs = "ranger",
    case_weights = TRUE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_imp_rf <- function(object, x, y, seed = sample.int(1000, 1), ...) {
  rlang::check_installed(object$pkgs)
  dat <- apply_data_filters(object, x, y)

  p <- ncol(dat$x)

  # check dots for 'importance'; when not set or use 'none'
  cl <- rlang::call2("ranger", .ns = "ranger", x = quote(dat$x), y = quote(dat$y[[1]]),
                     importance = "impurity_corrected", ...)
  set.seed(seed) # TODO use withr::with_seed?
  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- list(variable.importance = rep(NA_real_, p))
    names(res$variable.importance) <- names(dat$x)
  }

  scores <- score_vec(unname(res$variable.importance), impute = Inf)

  res <- new_filter_results(names(res$variable.importance), scores, object)
  res
}

###


#' @rdname filter_imp_rf
#' @export
filter_imp_crf <-
  new_filter_method(
    name = "imp_crf",
    label = "Random Forest Variable Importance (Conditional Inference)",
    predictor_types = c("numeric", "double", "integer", "factor"),
    outcome_types = c("numeric", "double", "integer", "factor"),
    pkgs = "party",
    case_weights = TRUE
  )

#' @rdname fit_xy.filter_method_corr
#' @export
#' @keywords internal
fit_xy.filter_method_imp_crf <- function(object, x, y, seed = sample.int(1000, 1), ...) {
  rlang::check_installed(object$pkgs)

  dat <- apply_data_filters(object, x, y)
  dat <- dplyr::bind_cols(dat$x, dat$y)
  f <- paste0(names(y), "~.")
  f <- as.formula(f)

  y <- y[[1]]
  p <- ncol(dat$x)

  cl_fit <- rlang::call2(
    "cforest",
    .ns = "party",
    formula = quote(f),
    data = quote(dat),
    ...
  )
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

  res <- new_filter_results(names(res), scores, object)
  res
}
