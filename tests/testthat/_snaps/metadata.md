# properties id.var, DUP_NO.ID, and DUP_FREQ works

    Code
      m@id.var <- c("a", "b")
    Condition
      Error:
      ! <epicdata::metadata>@id.var must have length 1 and must not be empty or NA

---

    Code
      m@id.var <- "a"
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - @id.var must be a variable specified in var.list

---

    Code
      m@DUP_NO.ID <- FALSE
    Condition
      Error:
      ! Can't set read-only property <epicdata::metadata>@DUP_NO.ID

---

    Code
      m@DUP_FREQ <- TRUE
    Condition
      Error:
      ! Can't set read-only property <epicdata::metadata>@DUP_FREQ

# property consent works

    Code
      m@consent.final <- TRUE
    Condition
      Error:
      ! Can't set read-only property <epicdata::metadata>@consent.final

---

    Code
      m@consent <- TRUE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - @consent can only be TRUE if @id.var has been specified

---

    Code
      m@id.var <- NULL
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - @consent can only be TRUE if @id.var has been specified

# touch.na works

    Code
      m@var.list$b$touch.na.default.option <- TRUE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.option directly. Change @touch.na instead.

---

    Code
      m@var.list$b$touch.na.default.option <- NULL
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.option directly. Change @touch.na instead.

---

    Code
      m@var.list$b$touch.na.default.option <- TRUE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.option directly. Change @touch.na instead.

---

    Code
      m@var.list$b$touch.na.default.group <- FALSE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.group directly. Change @var.groups instead.

---

    Code
      m@var.list$b$touch.na.default.group <- NULL
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.group directly. Change @var.groups instead.

---

    Code
      m@var.list$b$touch.na.default.group <- TRUE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.group directly. Change @var.groups instead.

---

    Code
      m@var.list$b$touch.na.default.group <- TRUE
    Condition
      Error:
      ! <epicdata::metadata> object is invalid:
      - Do not change @var.list$b$touch.na.default.group directly. Change @var.groups instead.

---

    Code
      m@touch.na <- 3
    Condition
      Error:
      ! <epicdata::metadata>@touch.na must be <NULL> or <logical>, not <double>

---

    Code
      m@touch.na <- NA
    Condition
      Error:
      ! <epicdata::metadata>@touch.na must have length 1 and must not be NA

---

    Code
      m@touch.na <- c(TRUE, FALSE)
    Condition
      Error:
      ! <epicdata::metadata>@touch.na must have length 1 and must not be NA

---

    Code
      m@touch.na <- c(TRUE, NA)
    Condition
      Error:
      ! <epicdata::metadata>@touch.na must have length 1 and must not be NA

---

    Code
      m@touch.na <- "TRUE"
    Condition
      Error:
      ! <epicdata::metadata>@touch.na must be <NULL> or <logical>, not <character>

---

    Code
      m@na.touch <- 3
    Condition
      Error:
      ! <epicdata::metadata>@na.touch must be <NULL> or <logical>, not <double>

---

    Code
      m@na.touch <- NA
    Condition
      Error:
      ! <epicdata::metadata>@na.touch must have length 1 and must not be NA

---

    Code
      m@na.touch <- c(TRUE, FALSE)
    Condition
      Error:
      ! <epicdata::metadata>@na.touch must have length 1 and must not be NA

---

    Code
      m@na.touch <- c(TRUE, NA)
    Condition
      Error:
      ! <epicdata::metadata>@na.touch must have length 1 and must not be NA

---

    Code
      m@na.touch <- "TRUE"
    Condition
      Error:
      ! <epicdata::metadata>@na.touch must be <NULL> or <logical>, not <character>

---

    Code
      m@var.list$a$touch.na <- 3
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$touch.na <- NA
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$touch.na <- c(TRUE, FALSE)
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$touch.na <- c(TRUE, NA)
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$touch.na <- "TRUE"
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$na.touch <- 3
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$na.touch <- NA
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$na.touch <- c(TRUE, FALSE)
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$na.touch <- c(TRUE, NA)
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.list$a$na.touch <- "TRUE"
    Condition
      Error:
      ! @var.list has touch.na (or na.touch) keys in the wrong format in variable 'a'

---

    Code
      m@var.groups$g$touch.na <- 3
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$touch.na <- NA
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$touch.na <- c(TRUE, FALSE)
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$touch.na <- c(TRUE, NA)
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$touch.na <- "TRUE"
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$na.touch <- 3
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$na.touch <- NA
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$na.touch <- c(TRUE, FALSE)
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$na.touch <- c(TRUE, NA)
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

---

    Code
      m@var.groups$g$na.touch <- "TRUE"
    Condition
      Error:
      ! @var.groups has touch.na (or na.touch) keys in the wrong format in group 'g'

