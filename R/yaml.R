read_meta <- function(file, testing = FALSE) {
  # Check input


  #=============================================================================
  x <- yaml::read_yaml(file)

  # Define name of column in meta data table main, where category names of integer variables are defined
  cat.col.name <- "values"

  # Process category names of integer variables
  for (i in 1:length(x$main)) {
    if ("values" %>% magrittr::is_in(names(x$main[[i]]))) {
      # Transform yaml like
      # values:
      # - 1: one or more
      # - 2: two
      # - 3: three
      # OR
      # values:
      # - 1: one or more
      #   2: two
      #   3: three
      # INTO
      # values:
      #   1: one or more
      #   2: two
      #   3: three
      if (checkmate::test_list(x$main[[i]][[cat.col.name]], types = "list")) {
        x$main[[i]][[cat.col.name]] %<>% yum::flatten_list_of_lists()
      }

      # Transform yaml like
      # values:
      #   1: one or more
      #   2: two
      #   3: three
      # INTO
      # values:
      # - 1=one or more
      # - 2=two
      # - 3=three
      if (checkmate::test_list(x$main[[i]][[cat.col.name]], types = "character")) {
        out <- vector(mode = "character", length = length(x$main[[i]][[cat.col.name]]))
        for (j in 1:length(out)) {
          out[j] <- paste0(names(x$main[[i]][[cat.col.name]])[j],"=",x$main[[i]][[cat.col.name]][[j]])
        }
        x$main[[i]][[cat.col.name]] <- out
      }

      # Transform yaml like
      # values:
      # - 1=one or more
      # - 2=two
      # - 3=three
      # INTO
      # values: 1=one or more|2=two|3=three
      if (checkmate::test_character(x$main[[i]][[cat.col.name]])) {
        x$main[[i]][[cat.col.name]] %<>%
          stringr::str_c(collapse = "|") %>%
          stringr::str_replace_all("[:blank:]*=[:blank:]*","=")
      }
    }
  }

  # !!!!
  # Make values transformation into a function
  # Then repeat the same for values_eng
  # !!!!

  # Transform corresponding elements to data.frames
  x %<>% purrr::map(\(x) if (is.list(x)) {
    purrr::map_dfr(x, as.data.frame)
  } else {
    as.character(x)
  })

  # Transform to epicdata_meta
  if (!testing) {
    x %<>% new_metadata()
  }

  # Validate
  if (!testing) {
    x %<>% validate_metadata()
  }

  return(x)
}
