#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.score_vec.score_vec <- function(x, y, ...) {
  atts <- c("direction", "impute")
  x_att <- attributes(x)[atts]
  y_att <- attributes(y)[atts]

  if (!identical(x_att, y_att)) {
    vctrs::stop_incompatible_type(x, y, x_arg = "", y_arg = "")
  }
  x
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.score_vec.numeric <- function(x, y, ...) {
  x
}


#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.score_vec.double <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.numeric.score_vec <- function(x, y, ...) {
  y
}

# ------------------------------------------------------------------------------

# Note order of class is the opposite as for ptype2
#' @exportS3Method vctrs::vec_cast
vec_cast.score_vec.score_vec <- function(x, to, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast
vec_cast.score_vec.numeric <- function(x, to, ...) {
  as_score_vec(x)
}

#' @exportS3Method vctrs::vec_cast
vec_cast.numeric.score_vec <- function(x, to, ...) {
  unclass(x)
}

#' @exportS3Method vctrs::vec_cast
vec_cast.double.score_vec <- function(x, to, ...) {
  unclass(x)
}

