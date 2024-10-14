#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom stats coef cor setNames as.formula

#' @importFrom pillar obj_sum
#' @export
pillar::obj_sum

#' @importFrom generics fit_xy
#' @export
generics::fit_xy

#' @importFrom generics tidy
#' @export
generics::tidy

## usethis namespace: end
NULL

# ------------------------------------------------------------------------------

# from tune
# nocov start

is_cran_check <- function () {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

#nocov end

# ------------------------------------------------------------------------------

utils::globalVariables(
  c("variable", "type", "score")
)

# ------------------------------------------------------------------------------

# TODOs
# - how to filter with non-informative results? specifically filter on informative
#   results and then keep the non-informative?
# - should we add main arguments for tuning parameters?
# - check for empty ellipses when required in fit_xy

