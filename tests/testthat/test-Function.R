context( "functions" )
sourceCpp( "testthat/cpp/Function.cpp" )

test_that( "Function correctly handles R objects", {
    expect_equal( function_( ls ), ls)
    expect_equal( function_( is.function ), is.function)
    
    expect_error( function_(1:10))
    expect_error( function_(TRUE))
    expect_error( function_(1.3))
    expect_error( function_(as.raw(1) ))
    expect_error( function_(new.env()))
})

test_that( "Function handles variable number of arguments", {
    expect_equal( function_variadic( sort, sample(1:20) ), 20:1)
    expect_error( function_variadic(sort, sort))
})

