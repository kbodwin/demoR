hlt_specific <- function(code_string, hltcolor = "red", specs) {
  ## Returns string of code_string with all (occurrences of) specs colored in hltcolor

  ## code_string must contain only code; no placeholders or other html

  #hlt_pieces <- unlist(map2(specs, hltcolor, txt_color))
  hlt_pieces <- txt_color(specs, hltcolor)

  print_string <- reduce2(c(code_string, specs), hlt_pieces, str_replace)

  paste("<pre class='r'><code>", print_string, "</code></pre>")
}


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

hlt_funs <- function(code_string, hltcolor = "red") {
  ## Returns string of code_string with all functions colored in hltcolor

  ## code_string must contain only code; no placeholders or other html

  strpieces <- strsplit(code_string, split = "\\(")[[1]]
  funs <- sapply(strsplit(strpieces[1:(length(strpieces)-1)], split = " "), function(x) x[length(x)])
  ## function names should always immediately preceed an open parentheses

  hlt_pieces <- sapply(funs, function(x) paste0("<font color='", hltcolor, "'>", x, "</font>"), USE.NAMES = FALSE)

  print_string <- reduce2(c(code_string, funs), hlt_pieces, str_replace)

  paste("<pre class='r'><code>", print_string, "</code></pre>")
}

hlt_vars <- function(code_string, hltcolor = "red") {
  ## Returns string of code_string with all variable inputs colored in hltcolor

  ## code_string must contain only code; no placeholders or other html

  strpieces <- strsplit(code_string, split = "=")[[1]]
  funs <- unlist(strsplit(strpieces[1:length(strpieces)], split = "\\)|, "))
  funs <- funs[grepl("^ ", funs)]
  funs <- trimws(funs[!grepl("[[:punct:]]", funs)])
  ## variable names should always immediately preceed a closed parentheses or comma,
  ## and immediately follow a space

  hlt_pieces <- sapply(funs, function(x) paste0("<font color='", hltcolor, "'>", x, "</font>"), USE.NAMES = FALSE)

  print_string <- reduce2(c(code_string, funs), hlt_pieces, str_replace)

  paste("<pre class='r'><code>", print_string, "</code></pre>")
}
