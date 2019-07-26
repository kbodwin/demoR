#' @export
create_demo <- function(label) {

  #### Cannibalized from `dance_start` in {matahari}

  editorIsOpen <- tryCatch({
    getSourceEditorContext()
    TRUE
  }, error = function(e) FALSE)

  if (editorIsOpen && isAvailable()) {
    ed <- getSourceEditorContext()
  } else {
    ed <- list(path = NA, contents = NA, selection = NA)
  }

  ####

  new_demo_code <- demo_code(get_my_code(ed$contents, label))

  return(new_demo_code)

}


get_my_code <- function(.contents, label) {


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
    trim()

  return(chunk_text)

}

