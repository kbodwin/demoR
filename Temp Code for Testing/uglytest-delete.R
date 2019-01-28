bridge <- function(code_string, user_inputs = list()){


  sym_inputs <- sub_syms(names(user_inputs))
  names(sym_inputs) <- names(user_inputs)

  code_string <- glue_data(sym_inputs, code_string)

  my_expr <- eval(parse(text = paste0("rlang::expr(",code_string,")")))

  eval(my_expr)
}

code_string <- "ggplot({dataset}) + {plot_type}(aes(x = {variable}))"

type <- "boxplot"

user_inputs <- list(dataset = dat, variable = var, plot_type = paste0("geom_", type))

bridge(code_string, user_inputs)

bridge(my_string, list(dataset = Input$dat, variable = var))



lapply(user_inputs, quo)


sub_syms <- function(my_list){

  lapply(my_list, function(x) paste0("(!!sym(user_inputs[['", x, "']]))"))

}


sub_print <- function(my_list, ...){

  lapply(my_list, function(x) txt_style(x, ...))

}

my_list <- list(dataset = "dat", variable = "var")

glue_data(sub_syms(my_list), my_string)
