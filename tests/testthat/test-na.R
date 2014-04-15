context("NA")
attributes::sourceCpp("cpp/na.cpp")

test_that("NA values are handled correctly", {
  expect_true( Rcpp_IsNA(NA_real_) )
  expect_true( Rcpp_IsNA(NA_real_ + 1) )
  expect_false( Rcpp_IsNA(0) )
  expect_false( Rcpp_IsNA(NaN) )
  expect_false( Rcpp_IsNA(0/0) )
  expect_false( Rcpp_IsNA(NaN + 1) )
  expect_false( Rcpp_IsNA(Inf) )
  expect_false( Rcpp_IsNA(-Inf) )
})

test_that("NaN values are handled correctly", {
  expect_true( Rcpp_IsNaN(NaN) )
  expect_true( Rcpp_IsNaN(0/0) )
  expect_false( Rcpp_IsNaN(NA_real_) )
  expect_false( Rcpp_IsNaN(NA_real_ + 1) )
  expect_false( Rcpp_IsNaN(Inf))
  expect_false( Rcpp_IsNaN(-Inf) )
})
