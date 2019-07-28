
<!-- README.md is generated from README.Rmd. Please edit that file -->

# demoR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/demoR)](https://cran.r-project.org/package=demoR)

## Overview

demoR is a set of tools for formatting `R` code for presentations,
teaching demonstrations, and much more:

  - The `hlt_*()` functions add text display formatting to printed code.

  - The `demo_*()` functions print and run the code.

These combine naturally within code chunks in R Markdown files via code
chunk options. You can learn more about them in `vignette("demoR")`.

## Installation

``` r
# The easiest way to get demoR is the basic installation:
install.packages("demoR")
```

### Development version

To get a bug fix, or use a feature from the development version, you can
install dplyr from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("kbodwin/demoR")
```

## Usage

``` r
library(demoR)
data(iris)
codestring <- "mean(iris$Sepal.Length)"

hlt_funs(codestring)
#> [1] "<pre class='prettyprint'><code><span style='background-color:#ffff7f'>mean</span>(iris$Sepal.Length)</code></pre>"

hlt_args(codestring)
#> [1] "<pre class='prettyprint'><code>mean(iris$Sepal.Length)</code></pre>"
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/kbodwin/demoR/issues). For questions and
other discussion, please use
[community.rstudio.com](https://community.rstudio.com/), or the
[manipulatr mailing list](https://groups.google.com/group/manipulatr).

-----
