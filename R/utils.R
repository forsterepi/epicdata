all_true <- function(x) {
  rlang::try_fetch(checkmate::assert_logical(x, any.missing = F, min.len = 1, null.ok = F),
            error = function(cnd) {
              cli::cli_abort("{.var x} must be a vector of TRUE/FALSE!",
                             parent = cnd, class = "input_all_true")
            })
  x %>% magrittr::not() %>% sum() %>% magrittr::equals(0)
}

all_false <- function(x) {
  rlang::try_fetch(checkmate::assert_logical(x, any.missing = F, min.len = 1, null.ok = F),
                   error = function(cnd) {
                     cli::cli_abort("{.var x} must be a vector of TRUE/FALSE!",
                                    parent = cnd, class = "input_all_false")
                   })
  x %>% sum() %>% magrittr::equals(0)
}
