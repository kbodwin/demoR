test_that("split_sandwich pulls html apart", {

  highlighted_code <- "<pre class='r'><code>foo <- mean(<span style='background-color:#ffff7f'>1:10</span>)</code></pre>"

  start_sym <- "\\>"
  end_sym <- "\\<"

  ignore_these <- c("\\<(?=(\\-|\\<))")

  expected_results_1 <- c(
    "<pre class='r'>",
    "<code>",
    "foo <- mean(",
    "<span style='background-color:#ffff7f'>",
    "1:10",
    "</span>",
    ")",
    "</code>",
    "</pre>")

  expect_equal(expected_results_1, split_sandwiches(highlighted_code, start_sym, end_sym, ignore_these))
})
