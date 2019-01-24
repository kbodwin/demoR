library(tidyverse)
library(patchwork)

ggcompare <- function(code_string, aes_option) {
  ## Plots two ggplots side-by-side
  ## Plot 1 is the original code string, without aes_option
  ## Plot 2 is the same ggplot with aes_option

  p1string <- paste(code_string, "+ ggtitle('Without", aes_option, "')")
  p1 <- eval(eval(parse(text = paste0("rlang::expr(",p1string,")"))))

  strpieces <- strsplit(code_string, split = "aes\\(")[[1]]
  p2string <- Reduce(paste0, c(paste0(strpieces[1], "aes(", aes_option, ", "), strpieces[-1]))
  p2string <- paste(p2string, "+ ggtitle('With", aes_option, "')")
  p2 <- eval(eval(parse(text = paste0("rlang::expr(", p2string, ")"))))

  ## From patchwork package
  p1 + p2
}




code_string <- "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point()"
aes_option <- "color = cyl"

