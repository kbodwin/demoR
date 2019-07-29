
<!-- README.md is generated from README.Rmd. Please edit that file -->
demoR
=====

To get started, please refer to [this vignette](https://web.calpoly.edu/~kbodwin/demoR/articles/demoR.html).

<!-- badges: start
[![CRAN status](https://www.r-pkg.org/badges/version/demoR)](https://cran.r-project.org/package=demoR)-->
Overview
--------

demoR is a set of tools for formatting `R` code for presentations, teaching demonstrations, and much more:

-   The `demo_*()` functions prepare your source code to be both evaluated and displayed by `knitr::knit()`.

-   The `hlt_*()` functions add text display formatting to printed code.

These combine naturally within code chunks in R Markdown files via code chunk options. You can learn more about them in `vignette("demoR")`.

Installation
------------

<!--

```r
# The easiest way to get demoR is the basic installation:
install.packages("demoR")
```
Not on CRAN yet!
-->
### Development version

Currently, only the development version of demoR is available. You can install this by running

``` r
# install.packages("devtools")
devtools::install_github("kbodwin/demoR")
```

Usage
-----

``` r
library(demoR)
data(iris)
codestring <- "mean(iris$Sepal.Length, na.rm = TRUE)"

hlt_funs(codestring)
#> [1] "<pre class='prettyprint'><code><span style='background-color:#ffff7f'>mean</span>(iris$Sepal.Length, na.rm = TRUE)</code></pre>"

hlt_args(codestring)
#> [1] "<pre class='prettyprint'><code>mean(iris$Sepal.Length, <span style='background-color:#ffff7f'>na.rm</span> = TRUE)</code></pre>"

demo_code(codestring) %>% hlt_args()
```

<pre class='prettyprint'><code>mean(iris$Sepal.Length, <span style='background-color:#ffff7f'>na.rm</span> = TRUE)</code></pre>
    #> [1] 5.843333

Getting help
------------

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/kbodwin/demoR/issues).

<!-- This doesn't apply to us, right?
For questions and other discussion, please use [community.rstudio.com](https://community.rstudio.com/), or the [manipulatr mailing list](https://groups.google.com/group/manipulatr). -->

------------------------------------------------------------------------
