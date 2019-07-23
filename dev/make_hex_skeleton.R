library(tidyverse)
library(hexSticker)

p <- ggplot(aes(x = mpg, y = wt), data = mtcars)
p <- p + theme_void() + theme_transparent()

sticker(p, package="", p_size=35, p_x = 1, p_y = 1, p_color = "#000000",
        filename="./dev/sticker_base.png", h_color = "red",  h_fill = "#ffffff")
