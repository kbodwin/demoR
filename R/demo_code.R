#' Creates an object of the class \code{demo_code}
#'
#' \code{demo_code} objects are evaluated R code, returned from \code{evaluate::evaluate}, with an attached attribute called \code{print_string} which sets up fancy formatting for knitting.
#'
#' @param .code_string A string containing executable R code OR a valid expression that will be converted to a string via \code{deparse()}
#' @param eval A boolean specifying whether the code should be immediately evaluated, in addition to creating the \code{demo_code} object. (Defaults to \code{TRUE})
#'
#' @return A \code{demo_code} object.
#'
#' @seealso \code{\link{highlight}}, \code{\link{demo_chunk}}
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
#' @importFrom stringr str_trim str_detect str_replace_all
#' @importFrom purrr map map_lgl quietly
#'
#' @export
demo_code <- function(.code_string, eval = TRUE, shatter = TRUE) {

  .code_string <- str_trim(.code_string)

  new_demo_code <- quietly(evaluate::evaluate)(.code_string)$result

  valid <- map_lgl(new_demo_code, ~ (class(.x) != "source") || str_detect(.x, "[^\\s]+"))

  new_demo_code <- new_demo_code[valid]

  is_src <- map(new_demo_code, class) == "source"

  # Scope and run it

  if (eval) {

    map(new_demo_code[is_src],
        ~quietly(scope_and_run)(.x))

  }

  new_demo_code[is_src] <- new_demo_code[is_src] %>%
    unlist() %>%
    str_replace_all(fixed("\n"), "<br>")


  attributes(new_demo_code) <- NULL

  if (!shatter) {

    .code_string <- str_replace_all(.code_string, fixed("\n"), "<br>")

    new_demo_code <- c(.code_string,
                       new_demo_code[!is_src])

    attr(new_demo_code, "where_sources") <- 1

  } else {


    attr(new_demo_code, "where_sources") <- which(is_src)

  }


  attr(new_demo_code, "class") <- "demo_code"

  attr(new_demo_code, "origin") <- "direct-string"

  attr(new_demo_code, "eval") <- eval

  get_doc_type <- purrr::safely(rmarkdown::all_output_formats)(knitr::current_input())

  if (!is.null(get_doc_type$error)) {

    attr(new_demo_code, "doc_type") <- "active_source"

  } else {

    attr(new_demo_code, "doc_type") <- get_doc_type$result

  }

  return(new_demo_code)

}


#' S3 method for knitting a \code{demo_code} object
#'
#' @importFrom purrr map
#'
#' @export
knit_print.demo_code <- function(x, ...) {

  where_sources <- attr(x, "where_sources")

  x[-where_sources] <- purrr::map(x[-where_sources], function(val) knitr:::wrap(val, ...))

  x[where_sources] <- purrr::map(x[where_sources], function(val) wrap_source(val, attr(x, "doc_type"), ...))

  if (!attr(x, "eval")) {

    x <- x[where_sources]

  }

  x <- x %>%
      str_c(collapse = " ")


  knitr::asis_output(x)

  #knitr::knit_print(unclass(x))

}

#' Helper for \code{knit_print.demo_code}
wrap_source <- function(x, doc_type, ...) {

  if (doc_type == "html_document") {

    x <- paste0("<pre class='prettyprint'>", txt_tocode(x), "</pre>")

  } else if (doc_type == "ioslides_presentation") {

    x <- paste0("<pre class='prettyprint lang-r'>", txt_tocode(x), "</pre>")

  } else if (doc_type == "xaringan::moon_reader") {

    x <- paste0("<code class ='r hljs remark-code'>", x, "</code>")

    } else if (doc_type == "slidy_presentation") {

      x <- paste0("<pre class='sourceCode r'><code class='sourceCode r'>", x, "</code></pre>")

    # } else if (doc_type == "revealjs::revealjs_presentation") {
    #
    #   x <-

    } else {

      x <- paste0("<pre><code class='language-r'>", txt_tocode(x), "</code></pre>")

    }

  return(x)

}

#' S3 method for printing a \code{demo_code}
#'
#' Prints nothing; \code{demo_code} objects should be seen and not heard.
#'
#' If the \code{demo_code} object was created by inputting a string, we should run that code and print any output.
#'
#' @export
print.demo_code <- function(x, ...) {

  # if code is being supplied as an input object, run things, with objects defined in global environment

  if (attr(x, "eval") && !isTRUE(getOption('knitr.in.progress'))) {

    where_sources <- attr(x, "where_sources")

    purrr::map(x[-where_sources], print)

  }

  #print(x)

}
