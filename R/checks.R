
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

check_ints <- function(x) {
  if (is.integer(x)) {
    x <- as.double(x)
  }
  x
}
