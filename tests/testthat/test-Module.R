context( "Modules" )
sourceCpp( "cpp/Module.cpp" )
    
test_that( "Module objects keep state in C++", {
    expect_equal( bar( 2L ), 4L )
    expect_equal( foo( 2L, 10.0 ), 20.0 )
    expect_equal( hello(), "hello" )
    
    w <- new( World )
    expect_equal( w$greet(), "hello" )
    w$set( "hello world" )
    expect_equal( w$greet(), "hello world" )
    w$set_ref( "hello world ref" )
    expect_equal( w$greet(), "hello world ref" )
    w$set_const_ref( "hello world const ref" )
    expect_equal( w$greet(), "hello world const ref" )
    w$clear( )
    expect_equal( w$greet(), "" )
})

test_that( "Module exposed classes can be used as parameters of functions", {
    test <- new( Test, 3.0 )
    expect_equal( Test_get_x_const_ref(test), 3.0 )
    expect_equal( Test_get_x_const_pointer(test), 3.0 )
    expect_equal( Test_get_x_ref(test), 3.0 )
    expect_equal( Test_get_x_pointer(test), 3.0 )
    
    expect_equal( attr_Test_get_x_const_ref(test), 3.0 )
    expect_equal( attr_Test_get_x_const_pointer(test), 3.0 )
    expect_equal( attr_Test_get_x_ref(test), 3.0 )
    expect_equal( attr_Test_get_x_pointer(test), 3.0 )
    
    expect_equal( test_reference( seq(0,10) ), 11L )
    expect_equal( test_const_reference( seq(0,10) ), 11L )
    expect_equal( test_const( seq(0,10) ), 11L )

})

test_that( "Module properties and fields work", {
    w <- new( Num )
    expect_equal( w$x, 0.0 )
    expect_equal( w$y, 0L )

    w$x <- 2.0
    expect_equal( w$x, 2.0 )

    expect_error( { w$y <- 3 } )

    w <- new( Number )
    expect_equal( w$x, 0.0 )
    expect_equal( w$y, 0L )

    w$x <- 2.0
    expect_equal( w$x, 2.0 )

    expect_error( { w$y <- 3 } )
})

test_that( "Module classes constructors work",{
    r <- new( Randomizer, 10.0, 20.0 )
    set.seed(123)
    x10 <- runif(10, 10.0, 20.0)
    set.seed(123)
    expect_equal(r$get(10), x10)
})

# test_that( "R classes can extend C++ classes", {
# 
#     td <- tempfile()
#     cwd <- getwd()
#     dir.create( td )
#     source <- file.path( "testthat", "testRcppClass" )
#     if( file.exists( source ) ){
#       file.copy(  , td, recursive = TRUE)
#       
#       setwd( td )
#       on.exit( { setwd( cwd) ; unlink( td, recursive = TRUE ) } )
#       R <- shQuote( file.path( R.home( component = "bin" ), "R" ))
#       cmd <- paste( R , "CMD build testRcppClass" )
#       system( cmd )
#       dir.create( "templib" )
#       install.packages( "testRcppClass_0.1.tar.gz", "templib", repos = NULL, type = "source" )            
#       
#       require( "testRcppClass", "templib", character.only = TRUE )
#       
#       v <- stdNumeric$new()
#       data <- as.numeric(1:10)
#       v$assign(data)
#       v$set(3L, v$at(3L) + 1)
#       
#       data[[4]] <- data[[4]] +1
#       
#       expect_equal( v$as.vector(), data )
#       
#       ## a few function calls
#       expect_equal( bar(2), 4)
#       expect_equal( foo(2,3), 6)
#    }
# })

