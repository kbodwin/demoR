bridge <- function(code_string, user_inputs = list()){

  # Check that user inputs is correct length
  placeholders <- code_string %>%
    str_extract_all("\\{[^\\}]*\\}") %>%
    unlist() %>%
    str_replace_all("\\}|\\{", '')

  n_ph <- length(placeholders)
  n_ui <- length(user_inputs)

  if (n_ph > n_ui){

    stop("Number of user inputs must match number of placeholders.")

  } else if (n_ph < n_ui) {

    warn(paste0("Number of user inputs (", n_ui, ") is greater than number of placeholders (", n_ph, "). Extras will be ignored."))
    user_inputs <- user_inputs[1:n_ph]

  }

  # For unnamed items in list, give names from unmatched placeholders, in order of appearance.
  unnamed <- map_lgl(names(user_inputs), ~.x == "")
  if (sum(unnamed) > 0){

    names(user_inputs)[unnamed] <- placeholders[!(placeholders %in% names(user_inputs))]

  }


  # Make list of html-wrapped user inputs
  col_inputs <- txt_color(user_inputs, "red")
  names(col_inputs) <- names(user_inputs)

  # Create string with html wrappers
  my_string <- glue_data(as.list(col_inputs), code_string) %>% txt_tocode()

  # Make a list of !!sym wrapped user inputs
  sym_inputs <- sub_syms(names(user_inputs))
  names(sym_inputs) <- names(user_inputs)

  # Create expression
  code_string <- glue_data(as.list(sym_inputs), code_string)
  my_expr <- eval(parse(text = paste0("rlang::expr(",code_string,")")))

  return(list(expr = my_expr, string = my_string))
}


sub_syms <- function(my_list){

  paste0("(!!sym(user_inputs[['", my_list, "']]))")

}


sub_print <- function(my_list, ...){

  lapply(my_list, function(x) txt_style(x, ...))

}
