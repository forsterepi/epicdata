# json: regex

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! In options, key `id.var` must contain a valid variable name.
      i Look at `?make.names` for details on syntactically valid names.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! In options, key `id.var` must contain a valid variable name.
      i Look at `?make.names` for details on syntactically valid names.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! In options, key `id.var` must contain a valid variable name.
      i Look at `?make.names` for details on syntactically valid names.

# json: errors for empty keys

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! Key options must have options specified below it. These options must be in a new line and indented by two spaces.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! Key options must have options specified below it. These options must be in a new line and indented by two spaces.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! var.list must have at least one variable specified. Variables must be in a new line and indented by two spaces. Variable-specific keys must again be in a new line and indented.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! var.list must have at least one variable specified. Variables must be in a new line and indented by two spaces. Variable-specific keys must again be in a new line and indented.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! var.groups must have at least one group specified. Groups must be in a new line and indented by two spaces. Group-specific keys must again be in a new line and indented.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! na.codes must have at least one NA code specified. NA codes must be integers between 1 and 9999 and in a new line. They form the keys while values must be strings describing their labels.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! import must have at least one imported dataset specified. Datasets must be in a new line and indented by two spaces. Dataset-specific keys must again be in a new line and indented.
      i Check `vignette(epicdata::metadata_long)` for more information.

# json: errors for contras

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! contras must be an array with string elements. Arrays are written as lists with dashes (`- element`).
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! contras must only have string elements. Try wrapping the elements in quotation marks if this error persists.
      i Check `vignette(epicdata::metadata_long)` for more information.

---

    Code
      metadata(file)
    Condition
      Error in `metadata()`:
      x The YAML input has an incorrect structure.
      ! contras must only have unique elements.
      i Check `vignette(epicdata::metadata_long)` for more information.

# json: errors for alias keys

    Code
      metadata(file)
    Condition
      Error in `dplyr::mutate()`:
      i In argument: `type = unlist(purrr::map(data, function(x) purrr::pluck(x, "type", .default = NA)))`.
      Caused by error:
      ! `type` must be size 1, not 2.

