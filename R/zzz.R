#' Sets hook options upon library load

.onLoad <- function(libname, pkgname) {

  hook_chunk <- knitr::knit_hooks$get('chunk')

  knitr::knit_hooks$set(chunk = function(x, options) {
    if (!is.null(options$demo)) {
      assign(options$demo, x, envir = .GlobalEnv)
    }

    hook_chunk(x, options)
  })

  invisible()
}


