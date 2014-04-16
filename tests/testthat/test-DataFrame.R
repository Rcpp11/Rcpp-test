
context( "data frame manipulation with Rcpp::DataFrame" )

    
test_that( "DataFrame can be constructed from a SEXP", {
    DF <- data.frame(a=1:3, b=c("a","b","c"))
    expect_equal( FromSEXP(DF), DF)
})

test_that( "DataFrame indexing works", {
    DF <- data.frame(a=1:3, b=c("a","b","c"))
    expect_equal( index_byName(DF, "a"), DF$a)
    expect_equal( index_byName(DF, "b"), DF$b)

    expect_equal( index_byPosition(DF, 0), DF$a)
    expect_equal( index_byPosition(DF, 1), DF$b)

    expect_equal( string_element(DF), as.character(DF[2,"b"]) )
})

test_that( "DataFrame::create works", {
    DF <- data.frame(a=1:3)
    expect_equal( createOne(), DF)

    DF <- data.frame(a=1:3, b=c("a","b","c"))
    expect_equal( createTwo(), DF)
    
    DF <- data.frame(a=1:3, b=c("a","b","c"), stringsAsFactors = FALSE )
    expect_equal( createTwoStringsAsFactors(), DF)
})

test_that( "DataFrame can be created from proxies", {
    suppressWarnings( setClass("track", representation(x="data.frame", y = "function"), where = environment()) )
    df <- data.frame( x = 1:10, y = 1:10 )
    tr1 <- new( "track", x = df, y = rnorm )
    expect_true( identical( SlotProxy(tr1, "x"), df ))
    expect_error(SlotProxy(tr1, "y"))

    tr1 <- structure( NULL, x = df, y = rnorm )
    expect_true( identical( AttributeProxy(tr1, "x"), df) )
    expect_error( AttributeProxy(tr1, "y"))
})

test_that( "DataFrame::nrows give the correct number of rows",{
    df <- data.frame( x = 1:10, y = 1:10 )
    expect_equal( DataFrame_nrows( df ), nrow(df) )
})

