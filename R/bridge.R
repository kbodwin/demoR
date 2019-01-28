bridge <- function(code_string, user_inputs = list()){

  col_inputs <- txt_color(user_inputs, "red")
  names(col_inputs) <- names(user_inputs)
  my_string <- glue_data(as.list(col_inputs), code_string) %>% txt_tocode()

  sym_inputs <- sub_syms(names(user_inputs))
  names(sym_inputs) <- names(user_inputs)

  code_string <- glue_data(sym_inputs, code_string)

  my_expr <- eval(parse(text = paste0("rlang::expr(",code_string,")")))

  return(list(expr = my_expr, string = my_string))
}


sub_syms <- function(my_list){

  lapply(my_list, function(x) paste0("(!!sym(user_inputs[['", x, "']]))"))

}


sub_print <- function(my_list, ...){

  lapply(my_list, function(x) txt_style(x, ...))

}
