context( "External pointers" )

    
test_that( "XPtr works",{
    xp <- xptr_1()
    expect_equal(typeof( xp ), "externalptr")
    
    front <- xptr_2(xp)
    expect_equal( front, 1L)
})

