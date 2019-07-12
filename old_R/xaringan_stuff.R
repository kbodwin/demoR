#### These are Garrick Aden-Buie's functions for highlighting

## Seems to rely on an automatic highlight that happens in js with * ?
## yep confirmed, * code automatically highlights that line in remark.js.  Not so in Markdown.  We'll have to go from scratch.
## But we should keep the #<< option and maybe the {{}} one?

# replace {{code}} with *code so that this line can be highlighted in remark.js;
# this also works with multiple lines
highlight_code = function(x) {
  x = paste0('\n', x)  # prepend \n and remove it later
  r = '(\n)([ \t]*)\\{\\{(.+?)\\}\\}(?=(\n|$))'
  m = gregexpr(r, x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    z = gsub(r, '\\1\\2\\3', z, perl = TRUE)  # remove {{ and }}
    z = gsub('\n', '\n*', z)     # add * after every \n
    z
  })
  x = gsub('^\n', '', x)
  # adds support for `#<<` line highlight marker at line end in code segments
  # catch `#<<` at end of the line but ignores lines that start with `*` since
  # they came from above
  x = gsub('^\\s?([^*].+?)\\s*#<<\\s*$', '*\\1', split_lines(x))
  paste(x, collapse = '\n')
}

highlight_output = function(x, options) {
  if (is.null(i <- options$highlight.output) || xfun::isFALSE(i)) return(x)
  x = split_lines(x)
  x[i] = paste0('*', x[i])
  paste(x, collapse = '\n')
}

# make sure blank lines and trailing \n are not removed by strsplit()
split_lines = function(x) {
  unlist(strsplit(paste0(x, '\n'), '\n'))
}
