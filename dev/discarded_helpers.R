split_in_out <- function(.string, start_sym, end_sym, split_blanks = FALSE) {
  
  if (split_blanks) {
    
    inner_regex <- glue::glue("{start_sym}[^({start_sym})|({end_sym})]+
  
  .string %>%
    str_extract_all(
  
}
