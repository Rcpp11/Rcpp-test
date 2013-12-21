library(testthat)
print(getwd())
print(dir(getwd()))
print(dir("testthat"))

test_check( "Rcpp" )

