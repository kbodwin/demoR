library(tidyverse)

hlt_args <- function(code_string, hltcolor = "red") {
  ## Returns string of code_string with all argument names colored in hltcolor

  ## code_string must contain only code; no placeholders or other html

  strpieces <- strsplit(code_string, split = "=")[[1]]
  funs <- unlist(strsplit(strpieces[1:length(strpieces)], split = "\\(|, "))
  funs <- trimws(funs[grepl(" $", funs)])
  ## argument names should always immediately follow an open parentheses or comma space,
  ## and immediately preceed a space equals

  hlt_pieces <- sapply(funs, function(x) paste0("<font color='", hltcolor, "'>", x, "</font>"), USE.NAMES = FALSE)

  print_string <- reduce2(c(code_string, funs), hlt_pieces, str_replace)

  paste("<pre class='r'><code>", print_string, "</code></pre>")
}











code_string <- "ggplot(data = df) + geom_histogram(aes(x = var1, y = var2))"
