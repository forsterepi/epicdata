# properties id.var, DUP_NO.ID, and DUP_FREQ works

    Code
      m@id.var <- c("a", "b")
    Condition
      Error:
      ! <epicdata::metadata>@id.var must have length 1

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

