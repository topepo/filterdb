#' @importFrom pillar obj_sum
#' @export
pillar::obj_sum

#' @importFrom generics fit_xy
#' @export
generics::fit_xy


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
