library(testthat)
sourceCpp <- function(file, ...){
  writeLines(head( readLines(file), 5 ))
  Rcpp::sourceCpp(file, ...)
}
test_check( "Rcpp" )



