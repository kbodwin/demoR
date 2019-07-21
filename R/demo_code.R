## an object of type demo_code is a language object that has a print_string attribute


demo_code <- function(.code, eval_here = TRUE) {

  code_expr <- rlang::enexpr(.code)

  if (eval_here) {

    code_expr %>%
        deparse() %>%
        str_replace_all("(?!=\\<)\\<\\-", "<<-") %>%
        str_c(collapse = "") %>%
        parse(text = .) %>%
        eval()

  }

  print_string <- code_expr %>%
    deparse() %>%
    str_c(collapse = "<br>") %>%
    str_replace_all("(^\\{\\s*)|(\\}$)", "") %>%
    txt_tocode()

  new_demo_code <- evaluate::evaluate(code_expr)

  attr(new_demo_code, "class") <- "demo_code"
  attr(new_demo_code, "print_string") <- print_string
  attr(new_demo_code, "expression") <- code_expr

  return(new_demo_code)

}

knit_print.demo_code <- function(x, ...) {

  if (length(x) > 1) {

    asis_output(paste(attr(x, "print_string"), knitr:::wrap(x[[2]], ...)))

  } else {

    asis_output(attr(x, "print_string"))

  }

}






# rx_tags <- "(\\<[^\\<\\>]*\\>)"
# rx_between <- "((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))"
