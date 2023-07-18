# Tools for a common API for filtering predictors

# ------------------------------------------------------------------------------
# Constructors

new_filter_method <- function(name, label, goal = "maximize",
                              inputs = "all", outputs = "all",
                              pkgs = character(0)) {

  # name: a keyword used in other steps (e.g. imp_rf? or similar)
  if (!is.character(name) || length(name) != 1) {
    rlang::abort("'name' should be a 1 element character vector.")
  }

  # ----------------------------------------------------------------------------
  # label: for printing ("random forest variable importance")
  if (!is.character(label) || length(label) != 1) {
    rlang::abort("'label' should be a 1 element character vector.")
  }

  # ----------------------------------------------------------------------------

  goal <- rlang::arg_match0(goal, c("maximize", "minimize", "zero"))

  # ----------------------------------------------------------------------------
  # Specifications for inputs and output variables
  # Maybe these should be more specific (e.g. "factor", "numeric", etc).
  # Should also specify max levels for factor inputs or outputs?
  inputs  <- rlang::arg_match0(inputs,  c("all", "qualitative", "quantitative"))
  outputs <- rlang::arg_match0(outputs, c("all", "qualitative", "quantitative"))

  # ----------------------------------------------------------------------------
  # pkgs: character string of external packages used to compute the filter
  if (!is.character(pkgs)) {
    rlang::abort("'pkgs' should be a character vector.")
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

new_filter_results <- function(predictors, results, object, num_pred, rename = FALSE) {
  # check dims and types
   if (!is.character(predictors)) {
     rlang::abort("'predictors' should be a character vector")
   }
  if (!is.numeric(results)) {
    rlang::abort("'results' should be a numeric vector")
  }
  if (length(predictors) != length(results)) {
    rlang::abort("'results' and 'predictors' should be the same length")
  }
  if (length(predictors) != num_pred) {
    msg <- paste(
      "'results' and 'predictors' should be the same length and the number of",
      "original predictors"
    )
    rlang::abort(msg)
  }

  # ------------------------------------------------------------------------------

  imp_val <- goal_to_impute(object)
  results <- new_score_vec(results, direction = object$goal, impute = imp_val)
  res <- dplyr::tibble(variable = predictors, score = results)

  if (object$goal == "minimize") {
    res <- res[order(res$score),]
  } else if (object$goal == "maximize") {
    res <- res[order(res$score, decreasing = TRUE),]
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

validate_filter_data <- function(object, x, y) {
  if (!is.data.frame(y)) {
    rlang::abort("'y' should be a data frame.")
  }
  if (nrow(x) != nrow(y)) {
    rlang::abort("The number of rows in 'x' and 'y' are not the same.")
  }
  if (ncol(y) > 1) {
    rlang::abort("Filters only support single outcome methods.")
  }

  bad_x <- is_bad_type(x, object$inputs)
  if (any(bad_x)) {
    msg <- paste("There are predictor columns that are not", object$inputs)
    rlang::abort(msg)
  }
  bad_y <- is_bad_type(y, object$outputs)
  if (any(bad_y)) {
    msg <- paste("There are outcome columns that are not", object$outputs)
    rlang::abort(msg)
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
    rlang::abort(paste0("Cannot have a score goal of '", x$goal, "'"))
  }
  res
}
