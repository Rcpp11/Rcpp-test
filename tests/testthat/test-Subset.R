context("Sugar subsetting")

test_that("Basic vector-vector subsetting works", {

  nv <- c(1, 2, 3, 4, 5)
  names(nv) <- letters[1:5]
  iv <- 1:5
  cv <- letters[1:5]
  lv <- c(TRUE, TRUE, FALSE, TRUE, FALSE)

  expect_equal(subset_test_int(nv, 0:2), 1:3)
  expect_equal(subset_test_lgcl(nv, lv), c(1, 2, 4))
  expect_equal(subset_test_char(nv, c('a', 'd', 'e')), c(1, 4, 5))

})

test_that("Integer subsetting fails when out of bounds", {
  expect_error(subset_test_int(1, -1))
  expect_error(subset_test_int(1, 1))
})

test_that("Logical subsetting fails when sizes differ", {
  expect_error(subset_test_lgcl(c(1, 2), TRUE))
  expect_error(subset_test_lgcl(c(1, 2), FALSE))
})

test_that("Logical subsetting fails with NAs", {
  expect_error(subset_test_lgcl(c(1, 2), c(TRUE, NA)))
})

test_that("x[x > 0] works", {
  expect_equal(subset_test_greater_0(as.numeric(-5:5)))
})

test_that("Subsetting with const char* works", {
  expect_equal(subset_test_literal(list(foo = "foo", bar = "bar")), list(foo = "foo"))
})

test_that("x[x > 0] = 0 works", {
  expect_equal(subset_test_assign(c(-2, -1, 0, 1, 2)), c(-2, -1, 0, 0, 0))
})
