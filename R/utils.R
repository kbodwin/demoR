#' @importFrom glue glue

split_sandwiches <- function(.string, start_sym, end_sym, ignore_these = NULL) {

  innards <- purrr::map(ignore_these, ~paste0("(", .x, ")")) %>%
    str_c(collapse = "|")


  # (?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$)
  # (?<=\\>|^)([^\\>]|(\\<(?=(\\-|\\<))))*(?=\\<|$)

  # wish I could {glue} this but ugh on \\
  meat <- paste0("(?<=", start_sym, "|^)([^", start_sym, "]|", innards, ")+(?=", end_sym, "|$)")

  bread <- paste0(end_sym, "[^", end_sym, start_sym, "]+", start_sym)


  full_regex = paste0("(", meat, ")|(", bread, ")")

  return(.string %>% str_extract_all(full_regex) %>% unlist())

  }



# Goal: turn assignments into global assignments, but not if they are in a function.

# scope_it <- function(.string) {
#
#   .string %>%
#
#
#   str_replace("(?!=\\<)\\<\\-", "<<-")
#
# }
