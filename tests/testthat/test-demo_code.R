test_that("code is scoped and run", {
  demo_code("foo <- 10")

  expect_equal(foo+10, 20)
})
