test_that('MRMR scores', {
  skip_if_not_installed("praznik")

  library(dplyr)

  data(credit_data, package = "modeldata")
  credit_data$Income <- as.double(credit_data$Income)

  mrmr_res <-
    fit_xy(filterdb:::filter_mrmr,
           x = credit_data %>% select(Marital, Time, Income),
           y = credit_data %>% select(Status))

  dat <-
    credit_data %>%
    select(Marital, Time, Income, Status) %>%
    mutate(
      Time = as.double(Time),
      Income = as.double(Income)
    )
  dat <- dat[complete.cases(dat),]
  res <- praznik::MRMR(X = dat[, 1:3], Y = dat$Status, k = 3)
  res <- tibble(variable = names(res$score), score = unname(res$score))
  res <- res[order(res$score, decreasing = TRUE), ]

  expect_equal(as.numeric(mrmr_res$score), res$score)
  expect_equal(mrmr_res$variable, res$variable)

  dat_na <-
    credit_data %>%
    select(Marital, Time, Income, Status) %>%
    mutate(
      Time = as.double(Time),
      Income = as.double(Income)
    )

  mrmr_na_res <-
    fit_xy(filterdb:::filter_mrmr,
           x = dat_na %>% select(Marital, Time, Income),
           y = dat_na %>% select(Status))

  expect_equal(mrmr_na_res, mrmr_res)

})
