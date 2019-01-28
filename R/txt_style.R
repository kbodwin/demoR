txt_style <- function(x,
                      font = NULL,
                      color = NULL,
                      size = NULL,
                      effects = NULL){

  if (!is.null(color)){
    x <- txt_color(x, color)
  }

  # if (!is.null(size)){
  #   x <- txt_color(x)
  # }
  #
  # if (!is.null(color)){
  #   x <- txt_color(x)
  # }




}

txt_color <- function(x, color){

  paste0("<font color='", color, "'>", x, "</font>")

}

txt_colour <- function(x, colour){
  txt_style(x, colour = color)
}

txt_font <- function(x, font){
  txt_style(x, font = font)
}

txt_size <- function(x, size){
  txt_style(x, size = size)
}

txt_tocode <- function(x){

  paste("<pre class='r'><code>", x, "</code></pre>")

}

