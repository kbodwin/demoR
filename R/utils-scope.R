#' Runs and prints from string, in parent environment
#'
#' Shortcut function to rescope a code string and then run and print output.  Looks for object assignments of the form \code{foo <-} and rescopes to \code{foo <<-}, then evaluates code string.
#'
#' @param .code_string A string containing runnable R code.
#'
#' @returns Nothing; side effects from \code{print()} only.
#'
scope_run_print <- function(.code_string) {


  # For object definitions, rescope but print nothing.
  if (stringr::str_detect(.code_string, "^[^\\s]+\\s*\\<\\-")) {

    .code_string %>%
      str_replace("(?!=\\<)\\<\\-", "<<-") %>%
      parse(text = .) %>%
      eval()

  } else {

    .code_string %>%
      parse(text = .) %>%
      eval() %>%
      print()

  }

}
