test_that('creating score vectors', {

  res_1 <- filterdb:::new_score_vec(1:5)
  expect_snapshot(unclass(res_1))
  res_2 <- filterdb:::new_score_vec(numeric(0))
  expect_snapshot(unclass(res_2))

  expect_snapshot_error(filterdb:::new_score_vec(letters))
  expect_snapshot_error(filterdb:::new_score_vec(1:2, direction = 2))
  expect_snapshot_error(filterdb:::new_score_vec(1:2, direction = "bigly"))
  expect_snapshot_error(filterdb:::new_score_vec(1:2, direction = NA_character_))
  expect_snapshot_error(filterdb:::new_score_vec(1:2, direction = character(0)))

  expect_snapshot_error(filterdb:::new_score_vec(letters, impute = "yes"))
  expect_snapshot_error(filterdb:::new_score_vec(letters, impute = NA_real_))
  expect_snapshot_error(filterdb:::new_score_vec(letters, impute = numeric(0)))

})


test_that('printing score vectors', {

  set.seed(1)
  res_1 <- filterdb:::new_score_vec(rnorm(3))

  expect_snapshot(print(res_1))
  expect_snapshot(print(res_1, digits = 10))
  expect_snapshot(format(res_1))
  expect_snapshot(obj_sum(res_1))

})
