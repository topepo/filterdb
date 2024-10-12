test_that('creating score vectors', {
  dbl_val <- seq(0, 1, length.out = 3)

  res_1 <- score_vec(dbl_val)
  expect_snapshot(unclass(res_1))
  res_2 <- score_vec(numeric(0))
  expect_snapshot(unclass(res_2))

  expect_snapshot(score_vec(letters), error = TRUE)
  expect_snapshot(score_vec(dbl_val, direction = 2), error = TRUE)
  expect_snapshot(score_vec(dbl_val, direction = "bigly"), error = TRUE)
  expect_snapshot(score_vec(dbl_val, direction = NA_character_), error = TRUE)
  expect_snapshot(score_vec(dbl_val, direction = character(0)), error = TRUE)

  expect_snapshot(score_vec(letters, impute = "yes"), error = TRUE)
  expect_snapshot(score_vec(letters, impute = NA_real_), error = TRUE)
  expect_snapshot(score_vec(letters, impute = numeric(0)), error = TRUE)

})


test_that('printing score vectors', {

  set.seed(1)
  res_1 <- score_vec(rnorm(3))

  expect_snapshot(print(res_1))
  expect_snapshot(print(res_1, digits = 10))
  expect_snapshot(format(res_1))
  expect_snapshot(obj_sum(res_1))

})


test_that('score vectors in filter results', {
  skip_if_not_installed("modeldata")
  library(dplyr)
  data(meats, package = "modeldata")

  corr_res <-
    fit_xy(filterdb:::filter_corr,
           x = meats %>% select(1:5),
           y = meats %>% select(water))

  expect_true(is_score_vec(corr_res$score))
  expect_equal(direction(corr_res$score), "maximize")
  expect_equal(missing_val(corr_res$score), Inf)

})


test_that('score vectors helpers', {

  set.seed(1)
  res_1 <- score_vec(rnorm(3))

  expect_snapshot(unclass(as_score_vec(1:5)))
  expect_snapshot(as_score_vec(letters), error = TRUE)
  expect_equal(as.numeric(as_score_vec(1:5)), 1:5)

  expect_snapshot(direction(1:3), error = TRUE)
  expect_equal(missing_val(as_score_vec(1:5, "zero", 0)), 0.0)
  expect_snapshot(missing_val(letters), error = TRUE)

  res_2 <- as_score_vec(1:5, "zero", 0)
  res_3 <- as_score_vec(c(1:5, NA_real_), "zero", 0)
  expect_snapshot(impute_score(letters), error = TRUE)
  expect_equal(impute_score(res_2), res_2)
  expect_equal(impute_score(res_3), as_score_vec(c(1:5, 0.0), "zero", 0))

  expect_true(is_score_vec(res_2))
  expect_false(is_score_vec(letters))

})
