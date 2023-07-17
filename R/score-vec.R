
# ------------------------------------------------------------------------------
# A new vctrs class for score values. This embeds the direction and imputation
# value into the numeric vector.

new_score_vec <-
  function(x, direction = "maximize", impute = Inf, ..., subclass = NULL) {

    stopifnot(is.numeric(x))

    stopifnot(is.character(direction) && length(direction) == 1L && !is.na(direction))
    direction <- rlang::arg_match0(direction, c("maximize", "minimize", "zero"))

    stopifnot(is.numeric(impute) && length(impute) == 1L && !is.na(impute))

    vctrs::new_vctr(
      .data = x,
      direction = direction,
      impute = impute,
      ...,
      class = c(subclass, "score_vec")
    )

  }

#' Create a filter score object
#'
#' `score_vec()` creates a `score_vec` object from a numeric vector. The
#' direction of best result (e.g. maximization) can be set along with a numeric
#' value to use when the value cannot be computed.
#'
#' direction values are those that you feel unsure about, and would like to
#' exclude from performance calculations or other metrics.
#'
#' @param x A numeric vector..
#' @param direction What is the best result? Possible values are "maximize",
#' "minimize", or "zero".
#' @param impute A single numeric value for when values are missing. This should
#' be a value that indicates that a variable _should not_ be removed.
#'
#' @examples
#' set.seed(1)
#' score_vec(rnorm(10), "zero")
#' @export
score_vec <-
  function(x = numeric(),
           direction = "maximize",
           impute = Inf) {

    # Check invariants
    if(!is.numeric(x)) {
      rlang::abort("`x` must be numeric.")
    }

    if(!rlang::is_scalar_character(direction)) {
      rlang::abort("`direction` must be a length 1 character.")
    }
    if(!rlang::is_scalar_integer(impute) & !rlang::is_scalar_double(impute)) {
      rlang::abort("`impute` must be a length 1 numeric")
    }

    x <- as.numeric(unclass(x))

    new_score_vec(
      x = x,
      direction = direction,
      impute = impute
    )
  }

# ------------------------------------------------------------------------------
# Printing

#' @export
print.score_vec <- function(x, digits = min(getOption("digits"), 3), ...) {
  print(as.numeric(x), digits = digits, ...)
  cat("Direction:", direction(x), "\n")
  invisible(x)
}

#' @export
format.score_vec <- function(x, digits = min(getOption("digits"), 3), ...) {
  format(as.numeric(x), digits = digits, ...)
}

#' @export
obj_sum.score_vec <- function(x) {
  dir_chr <- direction(x)
  if (nchar(dir_chr) > 4) {
    dir_chr <- substr(dir_chr, 1, 3)
  }
  paste0("score <", dir_chr, ">")
}



# ------------------------------------------------------------------------------
# Coercion

#' Coerce to a `score_vec` object
#'
#' `as_score_vec()` provides coercion to `score_vec` from other
#' existing objects.
#'
#' @inheritParams score_vec
#'
#' @examples
#' as_score_vec(1:4)
#' @export
as_score_vec <- function(x, direction = "maximize", impute = Inf) {
  UseMethod("as_score_vec")
}

quote_collapse <- function(x, quote = "`", collapse = ", ") {
  paste(encodeString(x, quote = quote), collapse = collapse)
}

abort_default <- function(x, fn) {
  cls <- quote_collapse(class(x))
  msg <- paste0("No implementation of `", fn, "()` for object of class ", cls, ".")
  rlang::abort(msg)
}

#' @export
as_score_vec.default <- function(x, direction = "maximize", impute = Inf) {
  abort_default(x, "as_score_vec")
}

#' @export
as_score_vec.numeric <- function(x, direction = "maximize", impute = Inf) {
  score_vec(x, direction, impute)
}

#' @export
as.numeric.score_vec <- function(x, ...) {
  as.numeric(unclass(x))
}

#' @export
as.double.score_vec <- function(x, ...) {
  as.double(unclass(x))
}

# ------------------------------------------------------------------------------
# Methods

# -----------------------
# direction
# missing_val

#' Return desired direction
#'
#'
#' @param x A `score_vec` object.
#'
#' @return A single character value (for the entire vector)
#'
#' @examples
#' set.seed(1)
#' roc_values <- score_vec(runif(10, min = 1 / 2), "maximize")
#' direction(roc_values)
#'
#' try(direction(1L))
#'
#' @name return-direction
#'
NULL

#' @rdname return-direction
#' @export
direction <- function(x) {
  UseMethod("direction")
}

#' @export
direction.default <- function(x) {
  abort_default(x, "direction")
}

#' @export
direction.score_vec <- function(x) {
  attr(x, "direction")
}

# -----------------------
# missing_val

#' @rdname return-direction
#' @export
missing_val <- function(x) {
  UseMethod("missing_val")
}

#' @export
missing_val.default <- function(x) {
  abort_default(x, "missing_val")
}

#' @export
missing_val.score_vec <- function(x) {
  attr(x, "impute")
}

# -----------------------
# impute_score

#' @rdname return-direction
#' @export
impute_score <- function(x) {
  UseMethod("impute_score")
}

#' @export
impute_score.default <- function(x) {
  abort_default(x, "impute_score")
}

#' @export
impute_score.score_vec <- function(x) {
  x[is.na(x)] <- missing_val(x)
  x
}

# -----------------------
# is_score_vec

#' Test if an object inherits from `score_vec`
#'
#' `is_score_vec()` checks if an object is a `score_vec` object.
#'
#' @param x An object.
#'
#' @examples
#'
#' x <- score_vec(1:5)
#'
#' is_score_vec(x)
#'
#' @export
is_score_vec <- function(x) {
  inherits(x, "score_vec")
}
