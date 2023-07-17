
library(tidymodels)
library(filterdb)
library(desirability2)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

top_p <- 3

# ------------------------------------------------------------------------------

data(cells)
cells$case <- NULL

# ------------------------------------------------------------------------------

# Look at how filters applied to different variable subsets work
vals_roc <- fit_xy(
  colino:::filter_roc_auc,
  x = cells %>% select(-ends_with("ch_1"), -class),
  y = cells %>% select(class)
)

vals_mrmr <- fit_xy(
  colino:::filter_mrmr,
  x = cells %>% select(-ends_with("ch_2"), -class),
  y = cells %>% select(class)
)

vals_gain <- fit_xy(
  colino:::filter_info_gain_ratio,
  x = cells %>% select(-class),
  y = cells %>% select(class)
)


vals_all <- join_scores(vals_roc, vals_gain, vals_mrmr, .impute = TRUE)

# ------------------------------------------------------------------------------
# Hard filter

filter_expr <- expr(roc_auc > .75 & (info_gain_ratio > 0.05 | mrmr > 0))
vals_all$filter_res <- rlang:::eval_tidy(filter_expr, vals_all)

# ------------------------------------------------------------------------------
# Simple ranking on one thing


vals_all %>%
  filter(!is.infinite(info_gain_ratio)) %>%
  slice_max(info_gain_ratio, n = top_p) %>%
  bind_rows(vals_all %>% filter(is.infinite(info_gain_ratio)))


# ------------------------------------------------------------------------------
# Ranking via desirability

filter_d <-
  ~ d_max(roc_auc, low = .5, high = 1) +
  d_max(info_gain_ratio, 0, 1) +
  d_max(mrmr, 0, 2)

model.frame(filter_d, vals_all) %>%
  as_tibble() %>%
  bind_cols(vals_all) %>%
  dplyr::mutate(.overall = d_overall(across(starts_with("d_")))) %>%
  slice_max(.overall, n = top_p) %>%
  select(-starts_with("d_"))
