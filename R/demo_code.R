## an object of type demo_code is a language object that has a print_string attribute

#' @export
demo_code <- function(.code_string, eval_here = TRUE) {

  .code_string <- str_trim(.code_string)

  if (eval_here) scope_and_run(.code_string)


  print_string <- .code_string %>%
    str_trim() %>%
    str_replace_all("\n", "<br>") %>%
    txt_tocode()

  new_demo_code <- evaluate::evaluate(str_trim(.code_string))

  is_output <- map(new_demo_code, class) != "source"

  new_demo_code <- new_demo_code[is_output]

  attr(new_demo_code, "print_string") <- print_string
  attr(new_demo_code, "class") <- "demo_code"

  return(new_demo_code)

}
#'
#' #' @export
#' demo_code_blob <- function(.code_string, eval_here = TRUE) {
#'
#'   if (eval_here) {
#'
#'     scope_and_run(.code_string)
#'
#'   }
#'
#'   print_string <- .code_string %>%
#'     str_trim() %>%
#'     str_replace_all("\n", "<br>") %>%
#'     txt_tocode()
#'
#'
#'   new_demo_code <- evaluate::evaluate(str_trim(.code_string))
#'
#'   is_output <- map(new_demo_code, class) != "source"
#'
#'   new_demo_code <- new_demo_code[is_output]
#'
#'   attr(new_demo_code, "print_string") <- print_string
#'   attr(new_demo_code, "class") <- "demo_code"
#'
#'   attr(dc_list, "class") <- "demo_code"
#'
#'   return(dc_list)
#'
#' }

#' @importFrom matahari dance_recital
#' @export
demo_code_multi <- function(.code_string, eval_here = TRUE, shatter = TRUE) {

  dc_list <- matahari::dance_recital(.code_string, evaluate = FALSE)

  if (eval_here) {

    purrr::map(dc_list$expr, scope_and_run)

  }

  .code_string <- .code_string %>%
    str_trim() %>%
    str_replace_all("\n", "<br>")

  if (shatter) {

    print_strings <- str_split(.code_string, "((\\<br\\>){2,})|(;(\\<br\\>)*)") %>% unlist() %>% as.list()

  } else {

    print_strings <- as.list(c(.code_string, rep(NULL, nrow(dc_list) - 1)))

  }

  dc_list$print_strings <- print_strings
  dc_list$evaluations <- map(dc_list$expr, ~evaluate::evaluate(.x))

  attr(dc_list, "class") <- "demo_code"

  return(dc_list)

}

# eval_and_wrap <- function(.expr, ...) {
#
#   .expr <- enexpr(.expr)
#   my_output <- evaluate::evaluate(.expr)
#
#   if (length(my_output) > 1) {
#
#     wrapped <- map(my_output[-1], function(x) knitr:::wrap(x, ...)) %>% str_c(collapse = "") %>% unlist()
#
#   } else {
#
#     wrapped <- ""
#
#   }
#
#   return(wrapped)
#
# }

#' @export
knit_print.demo_code <- function(x, ...) {

  #x$wrapped <- map(x$evaluations, ~map_if(.x, length(.x) > 1, function(val) knitr:::wrap(val, ...) %>% str_c(collapse = "")))


  # x$wrapped <- map(x$evaluations, function(val) if (length(val) > 2) knitr:::wrap(val[[2]], ...))
  #
  # to_print <- paste(unlist(x$print_string), unlist(x$wrapped)) %>% str_c(collapse = " ")
  #
  # asis_output(to_print)

  #to_print <- str_c(to_print, collapse = "<br>")

  if (length(x) > 0) {

    output_string <- map(x, function(val) knitr:::wrap(val, ...)) %>%
      str_c(collapse = " ")

    knitr::asis_output(paste(attr(x, "print_string"), output_string))

   } else {

     knitr::asis_output(attr(x, "print_string"))

   }

}

#' @export
# We don't want to print out any of the demo code info
# We do want to print the output of code

print.demo_code <- function(x, ...) {

  if (length(x) > 0) {

    map(x, print)

  }

}





# rx_tags <- "(\\<[^\\<\\>]*\\>)"
# rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"
