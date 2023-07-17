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

    is.numeric(x) is not TRUE

---

    is.character(direction) && length(direction) == 1L && !is.na(direction) is not TRUE

---

    `direction` must be one of "maximize", "minimize", or "zero", not "bigly".

---

    is.character(direction) && length(direction) == 1L && !is.na(direction) is not TRUE

---

    is.character(direction) && length(direction) == 1L && !is.na(direction) is not TRUE

---

    is.numeric(x) is not TRUE

---

    is.numeric(x) is not TRUE

---

    is.numeric(x) is not TRUE

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

