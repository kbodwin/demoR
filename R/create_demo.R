#' Builds a demo_code object from a code chunk
#'
#' This function reads the source code from a given code chunk that has the value \code{label} set to the \code{demo} option; i.e., \code{{r, demo = label}}.
#'
#' When run directly in a source file, \code{create_demo()} reads the text of the active file and extracts the relevant string of source code.  (Important: this only works in RStudio!)
#'
#' When run during the \code{knitr::knit()} process, \code{create_demo()} pulls the relevant chunk source during \code{knitr::knit_hooks$set("source").}
#'
#' @param label creates demo code object from a
#'
#' @return An object of class \code{\link{demo_code}}
#'
#' @importFrom rstudioapi isAvailable getSourceEditorContext
#'
#' @export
create_demo <- function(label) {

  # If RStudio is open, get source from current editor
  # If in knitr, use labelled chunk source

  if (isAvailable()) {

    editorIsOpen <- tryCatch({
      getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)
    if (editorIsOpen) {
      ed <- getSourceEditorContext()
    }
    else {
      ed <- list(path = NA, contents = NA, selection = NA)
    }

    sources <- ed$contents

    new_demo_code <- demo_code(code_from_editor(sources, label))
    attr(new_demo_code, "origin") <- "chunk-active"

  } else {

    sources <- get(label, envir = .GlobalEnv)

    if (is.null(sources)) {

      stop(paste0("Error: No demo chunk with label '", label, "'"))

    } else {

      new_demo_code <- demo_code(code_from_hook(sources))
      attr(new_demo_code, "origin") <- "chunk-knit"

    }


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
  chunk_regex <- paste0('\\`\\`\\`\\{r', '.*', ',demo=\\"', label, '\\".*\\}')

  start_chunk <- .contents %>%
    str_remove_all("\\s+") %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No demo chunk with label '", label, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate demo chunk label '", label, "'"))

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

#' Converts raw chunk to a string of code
#'
#' Raw chunk string has been pulled from within the \code{"source"} hook using \code{knitr::knit_hooks$get()}.  Backticks and trailing/leading lines are stripped, all formatting is otherwise perserved.
#'
#' @import stringr
code_from_hook <- function(.chunk) {

  chunk_text <- .chunk %>%
    str_split("\\`\\`\\`r?") %>%
    unlist() %>%
    str_subset("\\s*\\<[^\\<\\>]*\\>", negate = TRUE) %>%
    str_subset("[^\\s]") %>%
    str_c(collapse = "") %>%
    str_remove("^\n+") %>%
    str_remove("\n+$")

  return(chunk_text)
}


