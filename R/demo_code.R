#' Creates an object of the class \code{demo_code}
#'
#' \code{demo_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#' @param .code_string A string containing executable R code.
#' @param eval_here A boolean specifying whether the code should be immediately evaluated, in addition to creating the \code{demo_code} object. (Defaults to \code{TRUE})
#'
#' @return A \code{demo_code} object.
#'
#' @seealso \code{\link{hlt_*}}
#'
#' @examples
#'
#' # When run in console, this will print only the results of mean(1:10)
#' my_dc <- demo_code('mean(1:10)') %>% hlt_funs()
#'
#' # The demo_code object itself has no output
#'
#' my_dc
#'
#' # However, when knitted, the source code is formatted.
#'
#' attr(my_dc, "print_string")
#'
#'
#' # Objects defined in demo_code are created in the environment
#'
#' demo_code('foo <- mean(1:10)')
#'
#' foo + 5
#'
#'
#'
#' @export
demo_code <- function(.code_string, eval_here = TRUE) {

  .code_string <- str_trim(.code_string)

  print_string <- .code_string %>%
    str_trim() %>%
    str_replace_all("\n", "<br>") %>%
    txt_tocode()

  new_demo_code <- evaluate::evaluate(.code_string)

  is_output <- purrr::map(new_demo_code, class) != "source"

  new_demo_code <- new_demo_code[is_output]
  attributes(new_demo_code) <- NULL

  attr(new_demo_code, "print_string") <- print_string
  attr(new_demo_code, "class") <- "demo_code"

  return(new_demo_code)

}


#' S3 method for knitting a \code{demo_code} object

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

    output_string <- purrr::map(x, function(val) knitr:::wrap(val, ...)) %>%
      str_c(collapse = " ")

    knitr::asis_output(paste(attr(x, "print_string"), output_string))

   } else {

     knitr::asis_output(attr(x, "print_string"))

   }

}

#' S3 method for printing a \code{demo_code}
#'
#' Print results of evaluating sources.  Do NOT print any extra \code{demo_code} object info.
#'
#' @export

print.demo_code <- function(x, ...) {

  #print(attr(x, "sources"))
  map(attr(x, "sources"), scope_run_print)

}
