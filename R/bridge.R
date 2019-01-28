bridge <- function(code_string, user_inputs = list()){


  sym_inputs <- sub_syms(names(user_inputs))
  names(sym_inputs) <- names(user_inputs)

  code_string <- glue_data(sym_inputs, code_string)

  my_expr <- eval(parse(text = paste0("rlang::expr(",code_string,")")))

  my_expr
}


sub_syms <- function(my_list){

  lapply(my_list, function(x) paste0("(!!sym(user_inputs[['", x, "']]))"))

}


sub_print <- function(my_list, ...){

  lapply(my_list, function(x) txt_style(x, ...))

}
