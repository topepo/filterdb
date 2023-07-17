# creating score vectors

    Code
      unclass(res_1)
    Output
      [1] 1 2 3 4 5
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

    `x` must be numeric.

---

    `direction` must be a length 1 character.

---

    `direction` must be one of "maximize", "minimize", or "zero", not "bigly".

---

    is.character(direction) && length(direction) == 1L && !is.na(direction) is not TRUE

---

    `direction` must be a length 1 character.

---

    `x` must be numeric.

---

    `x` must be numeric.

---

    `x` must be numeric.

# printing score vectors

    Code
      print(res_1)
    Output
      [1] -0.626  0.184 -0.836
      Direction: maximize 

---

    Code
      print(res_1, digits = 10)
    Output
      [1] -0.6264538107  0.1836433242 -0.8356286124
      Direction: maximize 

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

    No implementation of `as_score_vec()` for object of class `character`.

---

    No implementation of `direction()` for object of class `integer`.

---

    No implementation of `missing_val()` for object of class `character`.

---

    No implementation of `impute_score()` for object of class `character`.

