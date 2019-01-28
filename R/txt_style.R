txt_style <- function(x,
                      font = NULL,
                      color = NULL,
                      size = NULL,
                      effects = NULL){




}

#txt_style(10, color = "red")

txt_color <- function(x, color){
  #txt_style(x, color = color)

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

