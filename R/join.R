#' Combine filter results
#'
#' Joins different results across variables.
#' @param ... A set of objects with class `filter_method`.
#' @param .impute A single logical to indicate whether missing data are converted
#' to a value that implies that a variable should be retained.
#' @export
join_scores <- function(..., .impute = FALSE) {
  sc_dfs <- list(...)
  good_cls <- purrr::map_lgl(sc_dfs, ~ inherits(.x, "filter_results"))
  if (any(!good_cls)) {
    rlang::abort("Data inputs must have class 'filter_results'.")
  }

  sc_nm <- purrr::map_chr(sc_dfs, ~ get_score_info(.x, "name"))
  sc_dfs <- purrr::map2_dfr(sc_dfs, sc_nm, ~ dplyr::mutate(.x, type = .y))

  sc_dfs <-
    tidyr::pivot_wider(
      sc_dfs,
      id_cols = variable,
      names_from = type,
      values_from = score
    )
  sc_dfs$.num_scores <- apply(sc_dfs[, -1], 1, function(x) sum(!is.na(x)))

  if (.impute) {
    for (i in sc_nm) {
      sc_dfs[[i]] <- impute_score(sc_dfs[[i]])
    }
  }

  sc_dfs
}

