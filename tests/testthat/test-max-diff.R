test_that('maximum difference scores', {

  library(dplyr)

  data(credit_data, package = "modeldata")

  # ------------------------------------------------------------------------------
  # numeric outcome

  diff_res <-
    fit_xy(filterdb:::filter_max_diff,
           x = credit_data %>% select(Home, Marital, Job),
           y = credit_data %>% select(Income))

  dat <-
    credit_data %>%
    select(Home, Marital, Job, Income)

  glm_obj <-
    purrr::map(dat[, 1:3],
               ~ glm(Income ~ . + 0,
                    data = tibble(x = .x, Income = dat$Income))
               )
  diffs <- purrr::map_dbl(glm_obj, ~ max(coef(.x)) - min(coef(.x)))
  diffs <- diffs[order(diffs, decreasing = TRUE)]

  expect_equal(as.numeric(diff_res$score), unname(diffs))
  expect_equal(diff_res$variable, names(diffs))

  # ------------------------------------------------------------------------------
  # binary outcome

  diff_res <-
    fit_xy(filterdb:::filter_max_diff,
           x = credit_data %>% select(Home, Marital, Job),
           y = credit_data %>% select(Records))

  dat <-
    credit_data %>%
    select(Home, Marital, Job, Records)

  glm_obj <-
    purrr::map(dat[, 1:3],
               ~ glm(Records ~ . + 0,
                     data = tibble(x = .x, Records = dat$Records),
                     family = binomial()))
  diffs <- purrr::map_dbl(glm_obj, ~ max(coef(.x)) - min(coef(.x)))
  diffs <- diffs[order(diffs, decreasing = TRUE)]

  expect_equal(as.numeric(diff_res$score), unname(diffs))
  expect_equal(diff_res$variable, names(diffs))

})
