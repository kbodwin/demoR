## an object of type demo_code is a language object that has a print_string attribute


demo_code <- function(.code, eval_here = TRUE) {

  code_expr <- rlang::enexpr(.code)


  if (eval_here) {

    code_expr %>%
      deparse() %>%
      str_replace("(?!=\\<)\\<\\-", "<<-") %>%
      parse(text = .) %>%
      eval()

  }

  print_string <- code_expr %>%
    deparse() %>%
    str_c(collapse = "") %>%
    str_replace_all("(^\\{\\s*)|(\\}$)", "") %>%
    txt_tocode()

  new_demo_code <- evaluate::evaluate(code_expr)

  attr(new_demo_code, "class") <- "demo_code"
  attr(new_demo_code, "print_string") <- print_string
  attr(new_demo_code, "expression") <- code_expr

  return(new_demo_code)

}

#demo_code({ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_point()})

## demo function prints the string and runs the code

# retrieve_demo <- function(envir) {
#
#   where_is <- map_lgl(ls(.GlobalEnv), ~class(env_get(.GlobalEnv, .x)) == "demo_code")
#   names_demos <- ls(.GlobalEnv)[where_is]
#   ps_demos <- map(names_demos, ~attr(env_get(.GlobalEnv, .x), "print_string"))
#
#   return(unlist(ps_demos))
# }

