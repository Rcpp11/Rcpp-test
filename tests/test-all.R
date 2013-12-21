library(testthat)
print(getwd())
print(dir(getwd()))
print(dir("testthat"))
print(dir("testthat/cpp"))

test_check( "Rcpp" )

