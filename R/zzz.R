# nocov start

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by [devtools::load_all()]
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}


.onLoad <- function(libname, pkgname) {

  s3_register("pillar::obj_sum", "score_vec")


  s3_register("generics::fit_xy", "filter_method_corr")
  s3_register("generics::fit_xy", "filter_method_corr_rank")
  s3_register("generics::fit_xy", "filter_method_imp_crf")
  s3_register("generics::fit_xy", "filter_method_imp_pls")
  s3_register("generics::fit_xy", "filter_method_imp_rf")
  s3_register("generics::fit_xy", "filter_method_info_gain")
  s3_register("generics::fit_xy", "filter_method_info_gain_ratio")
  s3_register("generics::fit_xy", "filter_method_max_diff")
  s3_register("generics::fit_xy", "filter_method_mic")
  s3_register("generics::fit_xy", "filter_method_mrmr")
  s3_register("generics::fit_xy", "filter_method_roc_auc")

}

# nocov end
