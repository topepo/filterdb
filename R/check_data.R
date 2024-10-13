
# TODO quick zv scan?
# TODO check binary outcome class?

check_data_for_filters <- function(info, x, y, call) {
  correct_data <-
    purrr::map_lgl(info,
                   ~ check_data_for_method(.x, x, y, verbose = TRUE, call = call))
  if (!any(correct_data)) {
    cli::cli_abort("There were no appropriate columns for any filter", call = call)
  }
  info[correct_data]
}

has_data_for_method <- function(mthd, x, y) {
  pred_cls <- mthd$predictor_types
  pred_cols <- has_data_classes(x, pred_cls)

  outcome_cls <- mthd$outcome_types
  outcome_cols <- has_data_classes(y, outcome_cls)
  # check for 2+ outcomes and fail

  list(predictors = pred_cols, outcomes = outcome_cols)
}


check_data_for_method <- function(mthd, x, y, verbose = TRUE,
                                  call = rlang::caller_env()) {
  res <- has_data_for_method(mthd, x, y)

  has_predictors <- length(res$predictors) > 0
  has_outcomes <- length(res$outcomes) > 0

  data_ok <- has_predictors & has_outcomes

  if (!verbose) {
    return(data_ok)
  }

  if (!has_predictors & !has_outcomes) {
    cli::cli_warn("There are no predictors with class(es)
                   {.val {mthd$predictor_types}} and no outcomes with class(es)
                   {.val {mthd$outcome_types}} for {mthd$label}.", call = call)
  } else if (!has_predictors) {
    cli::cli_warn("There are no predictors with class(es)
                  {.val {mthd$predictor_types}} for {mthd$label}.",
                  call = call)
  } else if (!has_outcomes) {
    cli::cli_warn("There are no outcomes with class(es)
                  {.val {mthd$outcome_types}} for {mthd$label}.",
                  call = call)
  }

  data_ok
}

has_col_class <- function(x, cls) {
  inherits(x, cls)
}

has_data_classes <- function(.data, cls) {
  res <- purrr::map_lgl(.data, ~ has_col_class(.x, cls))
  names(res)[res]
}

check_data_classes <- function(.data, cls) {
  has_cls <- has_data_classes(.data, cls)
  length(has_cls) > 0
}

apply_data_filters <- function(info, x, y) {
  # TODO check for data frame y with 1 column
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  cols <- has_data_for_method(info, x, y)
  # TODO cli stops when needed
  x <- x[, cols$predictors]
  y <- y[, cols$outcomes]
  list(x = x, y = y)
}

# ------------------------------------------------------------------------------

check_num_classes <- function(x, num_lvls = 2) {
  length(levels(x)) == num_lvls
}

check_unique_values <- function(x, num_vals = 5) {
  vctrs::vec_unique_count(x) >= num_vals
}

check_not_multiclass <- function(x) {
  # for regression and binary classification only
}


