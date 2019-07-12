## an object of type demo_code is a language object that has a print_string attribute


demo_code <- function(.code) {

  code_expr <- enexpr(.code)

  print_string <- code_expr %>%
    deparse() %>%
    str_c(collapse = "") %>%
    str_replace_all("(^\\{\\s*)|(\\}$)", "")


  attr(code_expr, "class") <- "demo_code"
  attr(code_expr, "print_string") <- print_string

  return(code_expr)

}

#demo_code({ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_point()})

## demo function prints the string and runs the code

demo <- function(my_demo_code) {

  my_demo_code <- enexpr(my_demo_code)

  eval(my_demo_code)
  return(my_demo_code)

}
