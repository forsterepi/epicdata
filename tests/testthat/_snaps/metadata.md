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

