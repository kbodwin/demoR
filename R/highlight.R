#' Highlights parts of a string
#'
#' \code{hlt_*} returns an string of R code with formatting wrappers (currently only html)
#'
#' @param .string A string object
#' @param pattern A regular expression to match
#' @param code Should this string be displayed in R code format?
#' @param hlt_color Color to highlight code with.  Defaults to
#' @param ... Formatting options, passed to \code{\link{txt_style}}
#'
#' @import stringr
# #

#' @export
hlt_regexp <- function(.string, pattern, code = TRUE, ...)  {
  UseMethod("hlt_regexp")
}

#' @export
hlt_regexp.demo_code = function(x, ...) {

  code_string <- attr(x, "print_string")
  attr(x, "print_string") <- hlt_regexp(code_string, ...)

  return(x)

}

#' @export
hlt_regexp.default <- function(.string, pattern, code = TRUE, ...) {
  ## Matches regular expression of pattern inside of code string
  ## Use fixed() to match exact string

  # We don't want to highlight existing tags
  ## extract html tag sequences, <*>
  ## extract things between html >*<

  # rx_tags <- "(\\<[^\\<\\>]*\\>)"
  # rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"

  split_string <- .string %>%
    str_extract_all("(\\<[^\\<\\>]*\\>)|((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))") %>%
    unlist()

  # < (not a bracket) >
  # OR
  # (start of string or >) then (no < unless part of <- or <-- assignments) then (end of string or <)

  which_tags <- split_string %>% str_detect("\\<[^\\-]") %>% unlist()

  .string <- purrr::map_if(split_string, !which_tags, function(x) hlt_quick(x, pattern, ...)) %>%
    unlist() %>%
    str_c(collapse = "")

  # wrap in code tags if needed
  if (code && !str_detect(.string, fixed("<code>"))) {
    .string <- txt_tocode(.string)
  }

  return(.string)
}

#' @export
hlt_quick <- function(.string, pattern, ...){

  if (length(list(...)) == 0) {
    .string <- .string %>% str_replace_all(pattern, txt_background)
  } else {
    .string <- .string %>% str_replace_all(pattern, function(x) txt_style(x, ...))
  }

  return(.string)
}

#' @export
hlt_all <- function(.string, ...)  {
  UseMethod("hlt_all")
}

#' @export
hlt_all.demo_code = function(x, ...) {

  code_string <- attr(x, "print_string")
  attr(x, "print_string") <- hlt_all(code_string, ...)

  return(x)

}

#' @export
hlt_all.default <- function(.string, ...) {

  start_rx = fixed("<pre class='prettyprint'><code>")
  end_rx = fixed("</code></pre>")

  split_string <- split_sandwiches(.string,
    start_rx, end_rx)

  which_tags <- str_detect(split_string, start_rx) | str_detect(split_string, end_rx)

  .string <- purrr::map_if(split_string, !which_tags,
                           function(x) hlt_quick(x, ".+", ...)) %>%
    unlist() %>% str_c(collapse = "")

  return(.string)

}


#' @export
hlt_fixed <- function(.string, pattern, ...) {

  hlt_regexp(.string, fixed(pattern), ...)

}

#' @export
hlt_args <- function(.string, ...) {

  ## argument names should always immediately follow an open parentheses or comma space, and immediately preceed a space equals
  # allows alphanumerics, _, and . in value name
  # Preceeded by:
  # Succeeded by: closed paren or comma
  arg_regexp <- "(?<=(\\(|, ?))([:alnum:]|_|\\.)+(?= ?\\=)"

  hlt_regexp(.string, arg_regexp, ...)

}

#' @export
hlt_funs <- function(.string, ...) {

  # allows alphanumerics, _, and . in value name
  # Succeeded by: open paren
  funs_regexp <- "([:alnum:]|_|\\.)+(?=\\()"

  hlt_regexp(.string, funs_regexp, ...)

}

#' @export
hlt_input_vals <- function(.string, ...) {

  # allows anything but a comma or close paren or equals or leading/trailing spaces
  # Preceeded by: equals and possibly space
  # Succeeded by: closed paren or comma
  ## OR
  # Preceeded by: open paren
  # Succeeded by: NOT an equals sign

  vars_regexp1 <- "(?<=\\= ?)[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?=(\\)|,))"
  vars_regexp2 <- "(?<=\\()[^,\\)\\= ][^,\\)\\=]*[^,\\)\\= ]*(?! ?\\=)"

  .string %>%
    hlt_regexp(vars_regexp1, ...) %>%
    hlt_regexp(vars_regexp2, ...)
}

#' @export
hlt_diff <- function(.string1, .string2, ...) {

  <script src="htmldiff.js"></script>
  ## need a function for string differences

}
