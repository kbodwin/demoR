#' Builds a \code{\link{demo_code}} object from a code chunk
#'
#' This function reads the source code from a given code chunk that has the value \code{label} set to the \code{demo} option; i.e., \code{{r, demo = "my_label"}}.
#'
#' When run directly in a source file, \code{demo_chunk()} reads the text of the active file and extracts the relevant string of source code from the labelled chunk.  (Important: this only works in RStudio.)
#'
#' When run during the \code{knitr::knit()} process, \code{demo_chunk()} pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param label String that gives the name of the label used in a chunk option. If left blank, current chunk is used.
#'
#' @return An object of class \code{\link{demo_code}}
#'
#' @importFrom stringr str_c str_trim
#'
#' @export
demo_chunk <- function(label) {

    sources = NULL

    try_chunk <- purrr::safely(knitr:::knit_code$get)(label)

    if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

      sources <- try_chunk$result %>%
        str_c(collapse = "\n") %>%
        str_trim()

      new_demo_code <- demo_code(sources)
      attr(new_demo_code, "origin") <- "chunk-knit"

    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

          editorIsOpen <- tryCatch({
            rstudioapi::getSourceEditorContext()
            TRUE
          }, error = function(e) FALSE)

          if (editorIsOpen) {
            ed <- rstudioapi::getSourceEditorContext()
            sources <- ed$contents

            new_demo_code <- demo_code(code_from_editor(sources, label))
            attr(new_demo_code, "origin") <- "chunk-active"
          }

    }

    if (is.null(sources)) {

      stop(paste0("Error: No chunk found with label '", label, "'"))

    }

  return(new_demo_code)

}

#' Converts raw editor text to a string of code
#'
#' Raw editor text has been taken from an active RStudio session via \code{rstudioapi::getSourceEditorContext()}.  Chunk delimiters and html is removed, all formatting is otherwise perserved.
#'
#' @importFrom stringr str_c str_which str_trim
code_from_editor <- function(.contents, label) {


  # Find the start of the desired demo chunk
  chunk_regex <- paste0('\\`\\`\\`\\{r ', label, '(\\}|(,.*\\}))$')

  start_chunk <- .contents %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with label '", label, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk label '", label, "'"))

  }

  end_chunk <- .contents[-c(1:start_chunk)] %>%
    str_which(fixed("```")) %>%
    min() + start_chunk

  chunk_text <- .contents[(start_chunk+1):(end_chunk-1)] %>%
    str_c(collapse = "\n") %>%
    str_trim()

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

