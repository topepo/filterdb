test_that('ROC scores', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("yardstick")

  library(dplyr)
  library(yardstick)

  data(cells, package = "modeldata")

  expect_snapshot(
    fit_xy(filterdb:::filter_roc_auc,
           x = cells %>% select(3:5),
           y = cells %>% select(class) %>% slice(1)),
    error = TRUE
  )

  expect_snapshot(
    fit_xy(filterdb:::filter_roc_auc,
           x = cells %>% select(1:5),
           y = cells %>% select(class)),
    error = TRUE
  )
  expect_snapshot(
    fit_xy(filterdb:::filter_roc_auc,
           x = cells %>% select(3:5),
           y = cells %>% select(6)),
    error = TRUE
  )
  expect_snapshot(
    fit_xy(filterdb:::filter_roc_auc,
           x = cells %>% select(3:5),
           y = cells$class),
    error = TRUE
  )

})
