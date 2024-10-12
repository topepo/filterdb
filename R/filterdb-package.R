#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom stats coef cor setNames as.formula
## usethis namespace: end
NULL


# ------------------------------------------------------------------------------

utils::globalVariables(c(
  c("variable", "type", "score")
))

# ------------------------------------------------------------------------------

# TODOs
# - how to filter with non-informative results? specifically filter on informative
#   results and then keep the non-informative?
# - should we add main arguments for tuning parameters?
# - check for empty ellipses when required in fit_xy

