# creating score vectors

    Code
      unclass(res_1)
    Output
      [1] 0.0 0.5 1.0
      attr(,"direction")
      [1] "maximize"
      attr(,"impute")
      [1] Inf

---

    Code
      unclass(res_2)
    Output
      numeric(0)
      attr(,"direction")
      [1] "maximize"
      attr(,"impute")
      [1] Inf

---

    Code
      score_vec(letters)
    Condition
      Error in `check_number_decimal_vec()`:
      ! `i` must be a number or `NA`, not the string "a".

---

    Code
      score_vec(dbl_val, direction = 2)
    Condition
      Error:
      ! `direction` must be a single string, not the number 2.

---

    Code
      score_vec(dbl_val, direction = "bigly")
    Condition
      Error in `new_score_vec()`:
      ! `direction` must be one of "maximize", "minimize", "maximize_abs", "minimize_abs", or "zero", not "bigly".

---

    Code
      score_vec(dbl_val, direction = NA_character_)
    Condition
      Error:
      ! `direction` must be a single string, not a character `NA`.

---

    Code
      score_vec(dbl_val, direction = character(0))
    Condition
      Error:
      ! `direction` must be a single string, not an empty character vector.

---

    Code
      score_vec(letters, impute = "yes")
    Condition
      Error in `check_number_decimal_vec()`:
      ! `i` must be a number or `NA`, not the string "a".

---

    Code
      score_vec(letters, impute = NA_real_)
    Condition
      Error in `check_number_decimal_vec()`:
      ! `i` must be a number or `NA`, not the string "a".

---

    Code
      score_vec(letters, impute = numeric(0))
    Condition
      Error in `check_number_decimal_vec()`:
      ! `i` must be a number or `NA`, not the string "a".

# printing score vectors

    Code
      print(res_1)
    Output
      [1] -0.626  0.184 -0.836
    Message
      Direction: "maximize"
      Imputed value: Inf

---

    Code
      print(res_1, digits = 10)
    Output
      [1] -0.6264538107  0.1836433242 -0.8356286124
    Message
      Direction: "maximize"
      Imputed value: Inf

---

    Code
      format(res_1)
    Output
      [1] "-0.626" " 0.184" "-0.836"

---

    Code
      obj_sum(res_1)
    Output
      [1] "score <max>"

# score vectors helpers

    Code
      unclass(as_score_vec(1:5))
    Output
      [1] 1 2 3 4 5
      attr(,"direction")
      [1] "maximize"
      attr(,"impute")
      [1] Inf

---

    Code
      as_score_vec(letters)
    Condition
      Error in `as_score_vec()`:
      ! No implementation of `as_score_vec()` for object of class "character".

---

    Code
      direction(1:3)
    Condition
      Error in `direction()`:
      ! No implementation of `direction()` for object of class "integer".

---

    Code
      missing_val(letters)
    Condition
      Error in `missing_val()`:
      ! No implementation of `missing_val()` for object of class "character".

---

    Code
      impute_score(letters)
    Condition
      Error in `impute_score()`:
      ! No implementation of `impute_score()` for object of class "character".

