
# Goal: turn assignments into global assignments, but not if they are in a function.

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


# rescope <- function(.code_string) {
#
#
#   # For object definitions, rescope but print nothing.
#   if (stringr::str_detect(.code_string, "^[^\\s]+\\s*\\<\\-")) {
#
#     .code_string %>%
#       str_replace("(?!=\\<)\\<\\-", "<<-")
#
#   } else {
#
#     .code_string
#
#   }
#
# }
