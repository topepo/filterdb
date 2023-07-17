#' Combine filter results
#'
#' Joins different results across variables.
#' @param ... A set of objects with class `filter_method`.
#' @param .impute A single logical to indicate whether missing data are converted
#' to a value that implies that a variable should be retained.
#' @export
join_scores <- function(..., .impute = FALSE) {
  sc_dfs <- list(...)
  sc_nm <- purrr::map_chr(sc_dfs, ~ get_score_info(.x, "name"))
  sc_goal <- purrr::map_chr(sc_dfs, ~ get_score_info(.x, "goal"))
  sc_default <- rep(Inf, length(sc_nm))
  sc_default <- ifelse(sc_goal == "minimize", -Inf, sc_default)
  sc_default <- ifelse(sc_goal == "zero", 0.0, sc_default)
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
    for (i in seq_along(sc_nm)) {
      col <- sc_nm[[i]]
      val <- sc_default[[i]]
      sc_dfs[[col]] <- dplyr::if_else(is.na(sc_dfs[[col]]), val, sc_dfs[[col]])
    }
  }

  sc_dfs
}

