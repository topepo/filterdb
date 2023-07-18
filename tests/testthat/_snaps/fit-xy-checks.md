# ROC scores

    Code
      fit_xy(filterdb:::filter_roc_auc, x = cells %>% select(3:5), y = cells %>%
        select(class) %>% slice(1))
    Error <rlang_error>
      The number of rows in 'x' and 'y' are not the same.

---

    Code
      fit_xy(filterdb:::filter_roc_auc, x = cells %>% select(1:5), y = cells %>%
        select(class))
    Error <rlang_error>
      There are predictor columns that are not quantitative

---

    Code
      fit_xy(filterdb:::filter_roc_auc, x = cells %>% select(3:5), y = cells %>%
        select(6))
    Error <rlang_error>
      There are outcome columns that are not qualitative

---

    Code
      fit_xy(filterdb:::filter_roc_auc, x = cells %>% select(3:5), y = cells$class)
    Error <rlang_error>
      'y' should be a data frame.

