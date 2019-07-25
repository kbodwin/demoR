#' @importFrom glue glue
#' @importFrom stringr str_extract_all str_subset str_length

#' @export
split_sandwiches <- function(.string, start_rx, end_rx = NULL) {

  #innards <- purrr::map(ignore_these, ~paste0("(", .x, ")")) %>%
   # str_c(collapse = "|")

  # wish I could {glue} this but ugh on \\
  #meat <- paste0("(?<=", last_start_sym, "|^)([^", start_sym, "]|", innards, ")+(?=", first_end_sym, "|$)")


  top_buns <- str_extract_all(.string, start_rx) %>% unlist()

  if (is.null(end_rx)) {

    meat <- .string %>%
      str_split(start_rx) %>% unlist()

    bottom_buns = NULL

  } else {

    meat <- .string %>%
      str_split(start_rx) %>% unlist() %>%
      str_split(end_rx) %>% unlist()

    bottom_buns <- str_extract_all(.string, end_rx) %>% unlist()

  }


  # Check that buns are matched, or if no buns just return the string
  if (!is.null(end_rx) && length(top_buns) != length(bottom_buns)) {

    stop("Error: Each top bread must have a matching bottom bread.")

  } else if (length(top_buns) == 0) {

    return(.string)

  }

  sammie <- make_sandwiches(meat, top_buns, bottom_buns)
  sammie <- sammie[str_length(sammie) != 0]

  return(sammie)

  }



make_sandwiches <- function(meat, top_buns, bottom_buns = NULL) {

    n_buns <- length(top_buns) + length(bottom_buns)
    n_meat <- length(meat)

    if (n_meat != n_buns + 1) {

      stop("Error: Something weird happened...")

    }

    if (is.null(bottom_buns)) {

      bread <- top_buns

    } else {

      bread <- rep("", n_buns)
      bread[((1:(n_buns/2))*2 - 1)] = top_buns
      bread[(1:(n_buns/2))*2] = bottom_buns

    }

    sammie <- rep("", n_meat + n_buns)
    where_meat <- (1:n_meat)*2 - 1
    sammie[where_meat] = meat
    sammie[-where_meat] = bread

    return(sammie)

}


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
