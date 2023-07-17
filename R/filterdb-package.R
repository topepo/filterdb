#' @keywords internal
"_PACKAGE"

## usethis namespace: start
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
# - filters specifically exclude or include?
# - use a new vcrts class to embed the non-informative value into the vector?
# - should we add main arguments for tuning parameters?
# - check for empty ellipses when required in fit_xy

