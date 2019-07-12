#' Wraps text in html or latex code for formatting
#'
#' \code{txt_style} returns a string including styling wrappers
#'
#' @param x The string to be wrapped
#' @param type The style of display, defaults to "html"  (currently nothing else is supported, sorry)
#' @param bold Should the text be bolded?
#' @param underline Should the text be underlined?
#' @param italics Should the text be italicized?
#' @param ... various display options: any html CSS \code{style} options, or one of \code{font}, \code{size}, \code{color}, \code{background}, \code{style}.
#'
#' @importFrom stringr str_c str_replace
#'

txt_style <- function(x, type = "html",
                      bold = FALSE, underline = FALSE, italics = FALSE, ...) {

  # Dots to list
  my_opts <- list(...)

  # If html to correct <span> options
  if (type == "html") {


    # replacements to pass to str_replace
    opts_to_html = c("^size" = "font-size",
                     "font$" = "font-family",
                     "^style" = "font-style",
                     "background$" = "background-color")

    names(my_opts) <- names(my_opts) %>%
      str_replace(fixed("_"), "-") %>%
      str_replace_all(opts_to_html)


    # check for bold, italics, underline

    if (bold) { my_opts <- c(my_opts, "text-weight" = "bold") }

    if (italics) { my_opts <- c(my_opts, "text-style" = "italics") }

    if (underline) { my_opts <- c(my_opts, "text-decoration" = "underline") }


    # put all options into <span>
    x <- wrap_html(x, my_opts)

  }

  return(x)

}


txt_color <- function(x, color = "red"){

  txt_style(x, color = color)

}

txt_colour <- function(x, colour = "red"){
  txt_style(x, color = colour)
}

txt_size <- function(x, size = "large"){
  txt_style(x, size = size)
}

txt_background <- function(x, bg_color = "#ffff7f"){

  txt_style(x, background = bg_color)

}

## Want this to auto-lighten background color
#
# txt_highlight <- function(x, hlt_color){
#
#   txt_style(x, background = hlt_color)
#
# }

txt_font <- function(x, font){
  txt_style(x, font = font)
}

txt_bold <- function(x) {
  txt_style(x, bold = TRUE)
}

txt_emph <- function(x) {
  txt_style(x, italics = TRUE)
}

txt_ul <- function(x) {
  txt_style(x, underline = TRUE)
}


txt_tocode <- function(x){

  paste0("<pre class='r'><code>", x, "</code></pre>")

}
