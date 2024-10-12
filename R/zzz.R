
.onLoad <- function(libname, pkgname) {

  vctrs::s3_register("pillar::obj_sum", "score_vec")

  vctrs::s3_register("generics::fit_xy", "filter_method_corr")
  vctrs::s3_register("generics::fit_xy", "filter_method_corr_rank")
  vctrs::s3_register("generics::fit_xy", "filter_method_imp_crf")
  vctrs::s3_register("generics::fit_xy", "filter_method_imp_pls")
  vctrs::s3_register("generics::fit_xy", "filter_method_imp_rf")
  vctrs::s3_register("generics::fit_xy", "filter_method_info_gain")
  vctrs::s3_register("generics::fit_xy", "filter_method_info_gain_ratio")
  vctrs::s3_register("generics::fit_xy", "filter_method_max_diff")
  vctrs::s3_register("generics::fit_xy", "filter_method_mic")
  vctrs::s3_register("generics::fit_xy", "filter_method_mrmr")
  vctrs::s3_register("generics::fit_xy", "filter_method_roc_auc")

}

# nocov end
