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

