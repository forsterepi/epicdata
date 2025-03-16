# property @name works

    Code
      variable(name = "if", type = "text")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @name must be a syntactically valid name

---

    Code
      variable(type = "text")
    Condition
      Error in `variable()`:
      ! @name is required

---

    Code
      variable(name = "", type = "text")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @name must be a syntactically valid name

---

    Code
      variable(name = NA, type = "text")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @name must be <character>, not <logical>

---

    Code
      variable(name = NULL, type = "text")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @name must be <character>, not <NULL>

---

    Code
      variable(name = c("a", "b"), type = "text")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @name must be length 1

---

    Code
      v@name <- NULL
    Condition
      Error:
      ! <epicdata::variable>@name must be <character>, not <NULL>

# property @type works

    Code
      variable(name = "id")
    Condition
      Error in `variable()`:
      ! @type is required

---

    Code
      variable(name = "id", type = "")
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @type must be one of 'text','cat','num','date','time','datetime'

---

    Code
      variable(name = "id", type = NA)
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @type must be <character>, not <logical>

---

    Code
      variable(name = "id", type = NULL)
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @type must be <character>, not <NULL>

---

    Code
      variable(name = "id", type = c("text", "cat"))
    Condition
      Error:
      ! <epicdata::variable> object properties are invalid:
      - @type must be length 1

---

    Code
      v@type <- NULL
    Condition
      Error:
      ! <epicdata::variable>@type must be <character>, not <NULL>

# property @old.id works

    Code
      v@old.id <- "id"
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @old.id must not be the same as @name

# properties @label and @label.eng work

    Code
      v@label.eng <- "the label"
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @label.eng can only be used together with @label

# properties @cats and @cats.eng work

    Code
      v@cats <- c("0=nein", "1=ja")
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats can only be used for variables of @type 'cat'

---

    Code
      v@cats.eng <- c("0=no", "1=yes")
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats.eng can only be used for variables of @type 'cat'

---

    Code
      v@cats.eng <- c("0=no", "1=yes", "2=maybe")
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats.eng must have levels identical to @cats

---

    Code
      v@cats.eng <- c("0=no", "2=maybe")
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats.eng must have levels identical to @cats

---

    Code
      v@type <- "num"
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats can only be used for variables of @type 'cat'

---

    Code
      v@cats <- c("0=nein", "ja=1", "2=vielleicht")
    Condition
      Error:
      ! <epicdata::variable> object is invalid:
      - @cats.eng must have levels identical to @cats

