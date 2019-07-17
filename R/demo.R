## an object of type demo_code is a language object that has a print_string attribute


demo_code <- function(.code) {

  code_expr <- rlang::enexpr(.code)

  print_string <- code_expr %>%
    deparse() %>%
    str_c(collapse = "") %>%
    str_replace_all("(^\\{\\s*)|(\\}$)", "") %>%
    txt_tocode()

  attr(code_expr, "class") <- "demo_code"
  attr(code_expr, "print_string") <- print_string

  # send the expression to a global variable, to be found by the hook

  current_demo_object <<- code_expr

  return(code_expr)

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

