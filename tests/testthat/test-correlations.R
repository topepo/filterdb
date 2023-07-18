test_that('correlation scores', {
  skip_if_not_installed("modeldata")

  library(dplyr)
  data(meats, package = "modeldata")

  corr_res <-
    fit_xy(filterdb:::filter_corr,
           x = meats %>% select(1, 20, 50, 100),
           y = meats %>% select(water))

  dat <- meats %>% select(water, 1, 20, 50, 100)
  corrs <- unname(sort(abs(cor(dat)[-1, 1]), decreasing = TRUE))
  expect_equal(as.numeric(corr_res$score), corrs)

  meats_na <- meats
  meats_na$x_001[1] <- NA_real_

  corr_na_res <-
    fit_xy(filterdb:::filter_corr,
           x = meats_na %>% select(1, 20, 50, 100),
           y = meats_na %>% select(water),
           rename = TRUE)

  dat_na <- meats_na %>% select(water, 1, 20, 50, 100)
  corrs_na <- abs(cor(dat_na, use = "pairwise.complete.obs")[-1, 1])
  corrs_na <- unname(sort(corrs_na, decreasing = TRUE))
  expect_equal(as.numeric(corr_na_res$corr), corrs_na)

})


test_that('rank correlation scores', {
  skip_if_not_installed("modeldata")

  library(dplyr)
  data(meats, package = "modeldata")

  corr_res <-
    fit_xy(filterdb:::filter_corr_rank,
           x = meats %>% select(1, 20, 50, 100),
           y = meats %>% select(water))

  dat <- meats %>% select(water, 1, 20, 50, 100)
  corrs <- unname(sort(abs(cor(dat, method = "spearman")[-1, 1]), decreasing = TRUE))
  expect_equal(as.numeric(corr_res$score), corrs)

  meats_na <- meats
  meats_na$x_001[1] <- NA_real_

  corr_na_res <-
    fit_xy(filterdb:::filter_corr_rank,
           x = meats_na %>% select(1, 20, 50, 100),
           y = meats_na %>% select(water),
           rename = TRUE)

  dat_na <- meats_na %>% select(water, 1, 20, 50, 100)
  corrs_na <- abs(cor(dat_na, use = "pairwise.complete.obs", method = "spearman")[-1, 1])
  corrs_na <- unname(sort(corrs_na, decreasing = TRUE))
  expect_equal(as.numeric(corr_na_res$corr_rank), corrs_na)

})
