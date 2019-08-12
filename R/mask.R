#' Blanks out part of the string
#'
#' @param .string A string object
#' @param pattern A regular expression to match
#' @param ... Further formatting options, passed to \code{\link{txt_style}}
#'
#' @export
mask <- function(.string, pattern, substitute = NA, ...) {

  mask_regexp(.string, fixed(pattern), substitute = substitute, ...)

}

#' @rdname mask
#' @export
mask_regexp <- function(.string, pattern, substitute = NA, ...) {

  if (is.na(substitute)) {
    hlt_regexp(.string, pattern, color = "transparent", ...)
  } else {
    num_pattern <- str_count(.string, pattern)

    if (length(substitute) == 1 & num_pattern == 1) { ## single replacement and single match?
      sub_patterns <- substitute
    } else if (length(substitute) == 1 & num_pattern > 1) { ## single replacement string to be enumerated?
      sub_patterns <- paste(substitute, 1:num_pattern)
    } else if (length(substitute) > 1) { ## recycle substitute to have num_pattern length
      sub_patterns <- substitute[rep(1:length(substitute), length = num_pattern)]
    }

    .string <- Reduce(function (strx, replacex) str_replace(strx, pattern, replacex),
                      c(.string, sub_patterns))
    hlt_regexp(.string, fixed(sub_patterns), ...)
  }
}
