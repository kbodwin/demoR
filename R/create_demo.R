#' @importFrom rstudioapi isAvailable getSourceEditorContext
#'
#' @export
create_demo <- function(label) {

  # If RStudio is open, get source from current editor

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

  } else {

  # Otherwise, hopefully we are in knitr.  Check for input file.

    #sources <- readLines(input2, encoding = 'UTF-8', warn = FALSE)
    #sources <- sources %>% str_split("\n")

    sources <- get(label, envir = .GlobalEnv)
    if (is.null(sources)) {

      stop(paste0("Error: No demo chunk with label '", label, "'"))

    } else {

      new_demo_code <- demo_code(code_from_hook(sources))

    }


  }

  return(new_demo_code)

}

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

code_from_hook <- function(.chunk) {

  .chunk <- "\n\n```r\nthing <-              1:10\n\n    plot(thing)  #commenting\n```\n\n<img src=\"test_demo_chunks_files/figure-html/unnamed-chunk-3-1.png\" width=\"672\" />\n\n```r\n#I am a comment\n```\n\n"

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


