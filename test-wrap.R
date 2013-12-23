context( "wrap" )
sourceCpp( "cpp/wrap.cpp", env = environment() )

test_that( "wrap handles std::map", {
  expect_equal(
    map_string_int(),
    c( a = 200L, b = 100L, c = 300L)
  )
  expect_equal(
    map_string_double(),
    c( a = 200, b = 100, c = 300)
  )
  expect_equal(
    map_string_bool(),
    c( a = FALSE, b = TRUE, c = TRUE )
  )
  expect_equal(
    map_string_Rbyte(),
    c( a = as.raw(1), b = as.raw(0), c = as.raw(2) )
  )
  expect_equal(
    map_string_string(),
    c( a = "bar", b = "foo", c = "bling" )
  )
  expect_equal(
    map_string_generic(),
    list( a = c(1L, 2L, 2L), b = c(1L, 2L), c = c(1L,2L,2L,2L) )
  )

  expect_equal( map_int_double(), c("-1" = 3, "0" = 2 ) )    
  expect_equal( map_double_double(), c("0" = 2, "1.2" = 3 ) )    
  expect_equal( map_int_vector_double(), list("0" = c(1,2), "1" = c(2,3) ) )    
  expect_equal(
    sapply( map_int_Foo(), function(.) .$get() ), 
    c("0" = 2, "1" = 3 )
  )    
  expect_equal( 
    sapply( vector_Foo(), function(.) .$get() ), 
    c(2, 3)
  )    
}) 

test_that( "wrap handles multimap", {
    expect_equal(
      multimap_string_int(),
      c( a = 200L, b = 100L, c = 300L)
    )
    expect_equal(
      multimap_string_double(),
      c( a = 200, b = 100, c = 300)
    )
    expect_equal(
      multimap_string_bool(),
      c( a = FALSE, b = TRUE, c = TRUE )
    )
    expect_equal(
      multimap_string_Rbyte(),
      c( a = as.raw(1), b = as.raw(0), c = as.raw(2) )
    )
    expect_equal(
      multimap_string_string(),
      c( a = "bar", b = "foo", c = "bling" )
    )
    expect_equal(
      multimap_string_generic(),
      list( a = c(1L, 2L, 2L), b = c(1L, 2L), c = c(1L,2L,2L,2L) )
    )
})

test_that( "wrap handles const char*", {
  expect_equal(nonnull_const_char(), "foo")
})

test_that( "wrap handles unordered_map", {
  res <- unordered_map_string_int()
  expect_equal( res[["a"]], 200L )
  expect_equal( res[["b"]], 100L )
  expect_equal( res[["c"]], 300L )

  res <- unordered_map_string_double()
  expect_equal( res[["a"]], 200 )
  expect_equal( res[["b"]], 100 )
  expect_equal( res[["c"]], 300 )

  res <- unordered_map_string_bool()
  expect_equal( res[["a"]], FALSE )
  expect_equal( res[["b"]], TRUE  )
  expect_equal( res[["c"]], TRUE  )

  res <- unordered_map_string_Rbyte()
  expect_equal( res[["a"]], as.raw(1) )
  expect_equal( res[["b"]], as.raw(0) )
  expect_equal( res[["c"]], as.raw(2) )

  res <- unordered_map_string_string()
  expect_equal( res[["a"]], "bar"   )
  expect_equal( res[["b"]], "foo"   )
  expect_equal( res[["c"]], "bling" )

  res <- unordered_map_string_generic()
  expect_equal( res[["a"]], c(1L,2L,2L)    )
  expect_equal( res[["b"]], c(1L,2L)       )
  expect_equal( res[["c"]], c(1L,2L,2L,2L) )
})

