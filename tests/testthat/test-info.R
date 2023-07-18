test_that('information gain scores', {
  skip_if_not_installed("FSelectorRcpp")

  library(dplyr)

  data(credit_data, package = "modeldata")

  info_res <-
    fit_xy(filterdb:::filter_info_gain,
           x = credit_data %>% select(Home, Marital, Job, Time),
           y = credit_data %>% select(Records))

  dat <-
    credit_data %>%
    select(Home, Marital, Job, Time, Records)

  info_vals <-
    purrr::map(dat[, 1:4],
               ~ FSelectorRcpp::information_gain(
                 y ~ .,
                 data = tibble(x = .x, y = dat$Records)
                 )
               )
  info_vals <- purrr::map_dbl(info_vals, ~ .x$importance)
  info_vals <- info_vals[order(info_vals, decreasing = TRUE)]

  expect_equal(as.numeric(info_res$score), unname(info_vals))
  expect_equal(info_res$variable, names(info_vals))

})

test_that('information gain ratio scores', {
  skip_if_not_installed("FSelectorRcpp")

  library(dplyr)

  data(credit_data, package = "modeldata")

  # ------------------------------------------------------------------------------
  # numeric outcome

  info_res <-
    fit_xy(filterdb:::filter_info_gain_ratio,
           x = credit_data %>% select(Home, Marital, Job, Time),
           y = credit_data %>% select(Records))

  dat <-
    credit_data %>%
    select(Home, Marital, Job, Time, Records)

  info_vals <-
    purrr::map(dat[, 1:4],
               ~ FSelectorRcpp::information_gain(
                 y ~ .,
                 data = tibble(x = .x, y = dat$Records),
                 type = "gainratio"
               )
    )
  info_vals <- purrr::map_dbl(info_vals, ~ .x$importance)
  info_vals <- info_vals[order(info_vals, decreasing = TRUE)]

  expect_equal(as.numeric(info_res$score), unname(info_vals))
  expect_equal(info_res$variable, names(info_vals))

})

test_that('maximal information coefficient scores', {
  skip_if_not_installed("minerva")

  library(dplyr)

  data(credit_data, package = "modeldata")

  set.seed(1)
  mic_res <-
    fit_xy(filterdb:::filter_mic,
           x = credit_data %>% select(Seniority, Age, Debt, Time),
           y = credit_data %>% select(Price))

  dat <-
    credit_data %>%
    select(Seniority, Age, Debt, Time, Price)

  set.seed(1)
  mic_vals <-
    purrr::map_dbl(dat[, 1:4],
                   ~ minerva::mine(.x, dat$Price, use = "pairwise.complete.obs")$MIC
    )
  mic_vals <- mic_vals[order(mic_vals, decreasing = TRUE)]

  expect_equal(as.numeric(mic_res$score), unname(mic_vals), tolerance = 0.1)
  expect_equal(mic_res$variable, names(mic_vals))

})

