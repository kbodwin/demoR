#' Creates an object of the class \code{demo_code}
#'
#' \code{demo_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#' @param .code_string A string containing executable R code OR a valid expression that will be converted to a string via \code{deparse()}
#' @param eval_here A boolean specifying whether the code should be immediately evaluated, in addition to creating the \code{demo_code} object. (Defaults to \code{TRUE})
#'
#' @return A \code{demo_code} object.
#'
#' @seealso \code{\link{hlt_*}}, \code{\link{create_demo}}
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

  as_expr <- rlang::enexpr(.code_string)

  if (!is.character(as_expr)) {

    .code_string <- deparse(.code_string) %>%
      str_remove("^\\{") %>%
      str_remove("\\}$")

    is_expr <- TRUE

  }


  .code_string <- str_trim(.code_string)

  print_string <- .code_string %>%
    str_trim() %>%
    str_replace_all("\n", "<br>") %>%
    txt_tocode()

  new_demo_code <- evaluate::evaluate(.code_string)

  is_output <- purrr::map(new_demo_code, class) != "source"

  new_demo_code <- new_demo_code[is_output]
  attributes(new_demo_code) <- NULL

  attr(new_demo_code, "class") <- "demo_code"

  attr(new_demo_code, "print_string") <- print_string

  if (is_expr) {

      attr(new_demo_code, "origin") <- "direct-expression"

  } else {

      attr(new_demo_code, "origin") <- "direct-string"

  }

  return(new_demo_code)

}


#' S3 method for knitting a \code{demo_code} object

#' @export
knit_print.demo_code <- function(x, ...) {

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
#' Prints nothing, \code{demo_code} objects should be seen and not heard.
#'
#' If the \code{demo_code} object was created by inputting a string, we should run that code.
#'
#' @export

print.demo_code <- function(x, ...) {

  # if code is being supplied as an input object,

  if (stringr::str_detect(attr(x, "origin"), "direct")) {

    scope_run_print(x

}
