library(tidyverse)

hlt_specific <- function(code_string, hltcolor = "red", specs) {
  ## Returns string of code_string with all (occurrences of) specs colored in hltcolor

  ## code_string must contain only code; no placeholders or other html

  hlt_pieces <- unlist(map2(specs, hltcolor,  function(x, y) paste0("<font color='", y, "'>", x, "</font>")))

  print_string <- reduce2(c(code_string, specs), hlt_pieces, str_replace)

  paste("<pre class='r'><code>", print_string, "</code></pre>")
}











code_string <- "ggplot(data = df) + geom_histogram(aes(x = var1))"
