
library(tidymodels)
library(filterdb)
library(desirability2)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

top_p <- 10

# ------------------------------------------------------------------------------

data(ames)
ames$Sale_Price <- log10(ames$Sale_Price)

ames_scores <-
  importance_metrics(
    ames %>% select(-Sale_Price),
    y = ames %>% select(Sale_Price),
    methods = c("corr", "corr_rank", "imp_rf")
  )

# ------------------------------------------------------------------------------
# filters for numeric predictors

vals_cor <- fit_xy(
  filterdb:::filter_corr_rank,
  x = ames %>% select(all_of(cols_num)),
  y = ames %>% select(Sale_Price)
)

vals_mic <- fit_xy(
  filterdb:::filter_mic,
  x = ames %>% select(all_of(cols_num)),
  y = ames %>% select(Sale_Price)
)

# ------------------------------------------------------------------------------
# filters for factor predictors

vals_max_diff <- fit_xy(
  filterdb:::filter_max_diff,
  x = ames %>% select(all_of(cols_fac)),
  y = ames %>% select(Sale_Price)
)

# ------------------------------------------------------------------------------
# all variables

vals_rf <- fit_xy(
  filterdb:::filter_imp_rf,
  x = ames %>% select(-Sale_Price),
  y = ames %>% select(Sale_Price)
)


# ------------------------------------------------------------------------------
# Combine

val_all <- join_scores(vals_rf, vals_max_diff, vals_cor, vals_mic, .impute = TRUE)

# ------------------------------------------------------------------------------
# Hard filter

val_all %>% filter(imp_rf > 4)

# ------------------------------------------------------------------------------
# Simple ranking on one thing


val_all %>%
  filter(!is.infinite(imp_rf)) %>%
  slice_max(imp_rf, n = top_p) %>%
  bind_rows(val_all %>% filter(is.infinite(imp_rf)))


# ------------------------------------------------------------------------------
# Ranking via desirability

# TODO may want use as.vector() in data before desirability

filter_d <-
  ~ d_max(corr_rank, 0.25, 1) +
  d_max(max_diff, 0, 3) +
  d_max(max_diff, 0.25, .60)

model.frame(filter_d, val_all) %>%
  as_tibble() %>%
  bind_cols(val_all) %>%
  dplyr::mutate(.overall = d_overall(across(starts_with("d_")))) %>%
  slice_max(.overall, n = top_p) %>%
  select(-starts_with("d_"))


