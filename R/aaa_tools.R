goal_types <- c("maximize", "minimize", "maximize_abs", "minimize_abs", "zero")

#' @include import-standalone-obj-type.R import-standalone-types-check.R
#' @keywords internal
#' @export
new_filter_method <- function(name, label, predictor_types, outcome_types, pkgs = character(0),
                              call = rlang::caller_env()) {
  check_string(name, allow_empty = FALSE, call = call)
  check_string(label, allow_empty = FALSE, call = call)

  if (!is.character(pkgs)) {
    cli::cli_abort("'pkgs' should be a character vector.", call = call)
  }

  # maybe also set default arguments and a list that can't be altered by the user?
  # maybe notes on case weights
  res <-
    list(
      name = make.names(name),
      label = tools::toTitleCase(label),
      predictor_types = predictor_types,
      outcome_types = outcome_types,
      pkgs = pkgs
    )
  class(res) <- c(paste0("filter_method_", name), "filter_method")
  res
}

# TODO case weights :-(

new_filter_results <- function(predictors, results, object, call = rlang::caller_env()) {
  # check dims and types
  if (!is.character(predictors)) {
    cli::cli_abort("{.arg predictors} should be a character vector", call = call)
  }
  if (!is.numeric(results)) {
    cli::cli_abort("{.arg results} should be a numeric vector", call = call)
  }
  if (length(predictors) != length(results)) {
    cli::cli_abort("{.arg results} and {.arg predictors} should be the same length",
                   call = call)
  }

  # ------------------------------------------------------------------------------

  if (!is_score_vec(results)) {
    cli::cli_abort("The scores must be a {.val score_vec} object.", call = call)
  }

  res <- dplyr::tibble(variable = predictors, score = results)

  base_cls <- class(res)
  attr(res, "spec") <- object # TODO only save what you need?
  class(res) <- c(paste0("filter_results", object$name), "filter_results", base_cls)
  res

}

# ------------------------------------------------------------------------------

check_number_decimal_vec <- function(x, ..., call = rlang::caller_env()) {
  if (!is.vector(x)) {
    cli::cli_abort("Value should be a vector.", call = call)
  }
  for (i in x) {
    check_number_decimal(i, ...)
  }
  invisible(NULL)
}

#' Find available importance filters
#'
#' This function shows the methods that can be used with [importance_metrics()].
#'
#' @details
#'
#' - `corr`: The correlation between numeric predictors and a numeric outcome.
#' - `corr_rank`: The Spearman (rank) correlation between numeric predictors and
#'    a numeric outcome.
#' - `imp_crf`: The unbiased variable importance score from a conditional
#'    inference random forest model. Any type of predictor or outcome can be used.
#' - `imp_pls`:
#' - `imp_rf`: The variable importance score from a standard random
#'    forest model. Any type of predictor or outcome can be used.
#' - `info_gain`:
#' - `info_gain_ratio`:
#' - `max_diff`:
#' - `mic`:
#' - `mrmr`:
#' - `roc_auc`: The area under the receiver operating characteristic (ROC) curve
#'    for numeric predictors and a binary outcome.
#'
#' @return A character vector of methods.
#' @export
#' @examples
#' filter_methods()
filter_methods <- function() {
  fit_methods <- as.character(utils::.S3methods("fit_xy"))
  fit_methods <- grep("fit_xy.filter_method_", fit_methods, value = TRUE)
  fit_methods <- gsub("fit_xy.filter_method_", "", fit_methods)
  fit_methods
}
