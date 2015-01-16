context( "strict" )

test_that( "Strict works", {
  x <- rnorm(10)
  expect_equal( test_Strict(x), sum(x) )

  x <- 1:10
  expect_error( test_Strict(x) )  
})

