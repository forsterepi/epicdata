# read_meta() shows correct error for empty yaml files

    Code
      read_meta(file)
    Condition
      Error in `read_meta()`:
      ! YAML must describe a single list!
      x Assertion on 'yaml_input' failed: Must be of type 'list', not 'NULL'.
      i Do not use dashes (`-`) in front of the main metadata components: options, var.list, etc.

