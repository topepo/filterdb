test_that('ROC scores', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("yardstick")

  library(dplyr)
  library(yardstick)

  data(cells, package = "modeldata")

  roc_res <-
    fit_xy(filterdb:::filter_roc_auc,
           x = cells %>% select(3:5),
           y = cells %>% select(class))

  dat_x <- cells %>% select(3:5)
  aucs <- purrr:::map_dbl(dat_x, ~ roc_auc_vec(truth = cells$class, .x))
  aucs <- ifelse(aucs < 0.5, 1 - aucs, aucs)
  aucs <- sort(aucs, decreasing = TRUE)
  aucs <- unname(aucs)
  expect_equal(as.numeric(roc_res$score), aucs)

  dat_na <- dat_x
  dat_na$avg_inten_ch_1[1] <- NA_real_

  roc_na_res <-
    fit_xy(filterdb:::filter_roc_auc,
           x = dat_na,
           y = cells %>% select(class),
           rename = TRUE)

  aucs_na <- purrr:::map_dbl(dat_na, ~ roc_auc_vec(truth = cells$class, .x))
  aucs_na <- ifelse(aucs_na < 0.5, 1 - aucs_na, aucs_na)
  aucs_na <- sort(aucs_na, decreasing = TRUE)
  aucs_na <- unname(aucs_na)
  expect_equal(as.numeric(roc_na_res$roc_auc), aucs_na)

})
