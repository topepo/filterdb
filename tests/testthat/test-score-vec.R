test_that('creating score vectors', {

  res_1 <- score_vec(1:5)
  expect_snapshot(unclass(res_1))
  res_2 <- score_vec(numeric(0))
  expect_snapshot(unclass(res_2))

  expect_snapshot_error(score_vec(letters))
  expect_snapshot_error(score_vec(1:2, direction = 2))
  expect_snapshot_error(score_vec(1:2, direction = "bigly"))
  expect_snapshot_error(score_vec(1:2, direction = NA_character_))
  expect_snapshot_error(score_vec(1:2, direction = character(0)))

  expect_snapshot_error(score_vec(letters, impute = "yes"))
  expect_snapshot_error(score_vec(letters, impute = NA_real_))
  expect_snapshot_error(score_vec(letters, impute = numeric(0)))

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
  expect_snapshot_error(as_score_vec(letters))
  expect_equal(as.numeric(as_score_vec(1:5)), 1:5)

  expect_snapshot_error(direction(1:3))
  expect_equal(missing_val(as_score_vec(1:5, "zero", 0)), 0.0)
  expect_snapshot_error(missing_val(letters))

  res_2 <- as_score_vec(1:5, "zero", 0)
  res_3 <- as_score_vec(c(1:5, NA_real_), "zero", 0)
  expect_snapshot_error(impute_score(letters))
  expect_equal(impute_score(res_2), res_2)
  expect_equal(impute_score(res_3), as_score_vec(c(1:5, 0.0), "zero", 0))

  expect_true(is_score_vec(res_2))
  expect_false(is_score_vec(letters))

})
