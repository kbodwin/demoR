#' demoR: A package for clear demonstration of R code.
#'
#' The demoR package provides ...
#'
#' @section Highlight functions:
#'
#' The \code{hlt_*} functions add formatting to printed code.
#'
#' @section Demo functions:
#'
#' The \code{demo_*} functions print and run the code.
#'
#' @docType package
#' @name demoR
NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
