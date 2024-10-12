var_types <- c("all", "qualitative", "quantitative") # TODO vectors of specific types (eg double, factor)
goal_types <- c("maximize", "minimize", "maximize_abs", "minimize_abs", "zero")

#' @include import-standalone-obj-type.R import-standalone-types-check.R
#' @keywords internal
#' @export
new_filter_method <- function(name, label, goal, inputs, outputs, pkgs = character(0),
                              call = rlang::caller_env()) {
  check_string(name, allow_empty = FALSE, call = call)
  check_string(label, allow_empty = FALSE, call = call)
  goal <- rlang::arg_match0(goal, goal_types, error_call = call)
  inputs  <- rlang::arg_match0(inputs, var_types , error_call = call)
  outputs <- rlang::arg_match0(outputs, var_types, error_call = call)
  if (!is.character(pkgs)) {
    cli::cli_abort("'pkgs' should be a character vector.", call = call)
  }

  # maybe also set default arguments and a list that can't be altered by the user?
  # maybe notes on case weights
  res <-
    list(
      name = make.names(name),
      label = tools::toTitleCase(label),
      goal = goal,
      inputs = inputs,
      outputs = outputs,
      pkgs = pkgs
    )
  class(res) <- c(paste0("filter_method_", name), "filter_method")
  res
}

# TODO case weights :-(

# TODO add imputation value
new_filter_results <- function(predictors, results, object, num_pred, rename = FALSE,
                               call = rlang::caller_env()) {
  # check dims and types
   if (!is.character(predictors)) {
     cli::cli_abort("{.arg predictors} should be a character vector", call = call)
   }
  if (!is.numeric(results)) {
    cli::cli_abort("{.arg results} should be a numeric vector", call = call)
  }
  if (length(predictors) != length(results)) {
    cli::cli_abort("{.arg results} and {.arg predictors} should be the same length", call = call)
  }
  if (length(predictors) != num_pred) {
    cli::cli_abort("{.arg results} and {.arg predictors} should be the same
                   length and the number of original predictors",
                   call = call)
  }

  # ------------------------------------------------------------------------------

  imp_val <- goal_to_impute(object)
  results <- new_score_vec(results, direction = object$goal, impute = imp_val)
  res <- dplyr::tibble(variable = predictors, score = results)

  # TODO make some sort of S3 method here (transform and/or ranking) There is a
  # base::transform method
  # TODO why even do this?
  if (object$goal == "minimize") {
    res <- res[order(res$score),]
  } else if (object$goal == "maximize") {
    res <- res[order(res$score, decreasing = TRUE),]
  } else if (object$goal == "minimize_abs") {
    res <- res[order(abs(res$score)),]
  } else if (object$goal == "maximize_abs") {
    res <- res[order(abs(res$score), decreasing = TRUE),]
  } else if (object$goal == "zero") {
    res <- res[order(abs(res$score)),]
  }

  if (rename) {
    names(res)[2] <- object$name
  }
  base_cls <- class(res)
  attr(res, "spec") <- object # TODO only save what you need?
  class(res) <- c(paste0("filter_results", object$name), "filter_results", base_cls)
  res

}

# ------------------------------------------------------------------------------
# Checks for data types required for the methods

is_qual <- function(x) {
  inherits(x, c("factor", "character"))
}

is_quant <- function(x) {
  inherits(x, c("numeric", "double", "integer"))
}

is_bad_type<- function(data, type) {
  if (type == "quantitative") {
    is_bad_type <- purrr::map_lgl(data, is_qual)
  } else if (type == "qualitative") {
    is_bad_type <- purrr::map_lgl(data, is_quant)
  } else {
    is_bad_type <- FALSE
  }
  is_bad_type
}

validate_filter_data <- function(object, x, y, call) {
  if (!is.data.frame(y)) {
    cli::cli_abort("{.arg y} should be a data frame.", call = call)
  }
  if (nrow(x) != nrow(y)) {
    cli::cli_abort("The number of rows in {.arg x} and {.arg y} are not the same.", call = call)
  }
  if (ncol(y) > 1) {
    cli::cli_abort("Filters only support single outcome methods.", call = call)
  }

  bad_x <- is_bad_type(x, object$inputs)
  if (any(bad_x)) {
    cli::cli_abort("There are predictor columns that are not {object$inputs}", call = call)
  }
  bad_y <- is_bad_type(y, object$outputs)
  if (any(bad_y)) {
    cli::cli_abort("There are outcome columns that are not {object$outputs}",  call = call)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' Get some attribute of a filter method
#'
#' @param x An object.
#' @param att A single character value for the attribute being returned.
#' @param ... Not currently used.
#' @export
get_score_info <- function(x, att = "goal", ...) {
  UseMethod("get_score_info")
}

#' @rdname get_score_info
#' @export
get_score_info.filter_method <- function(x, att = "goal", ...) {
  x[[att]]
}

#' @rdname get_score_info
#' @export
get_score_info.filter_results <- function(x, att = "goal", ...) {
  attributes(x)$spec[[att]]
}


goal_to_impute <- function(x) {
  if (x$goal == "maximize") {
    res <- Inf
  } else if (x$goal == "minimize") {
    res <- -Inf
  } else if (x$goal == "zero") {
    res <- 0.0
  } else {
    cli::cli_abort("Cannot have a score goal of {x$goal}", call = call)
  }
  res
}

check_number_decimal_vec <- function(x, ..., call = rlang::caller_env()) {
  if (!is.vector(x)) {
    cli::cli_abort("Value should be a vector.", call = call)
  }
  for (i in x) {
    check_number_decimal(i, ...)
  }
  invisible(NULL)
}

transform_score <- function(x, goal) {
  if (grepl("abs$", goal) | goal == "zero") {
    x <- abs(x)
  }
  x
}
