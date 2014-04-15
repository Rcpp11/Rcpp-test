
context( "Conversion from SEXP to C++ type with as<>" )


test_that( "as<> works for primitive types", {
  expect_equal( as_int(10)         , 10L)
  expect_equal( as_int(10L)        , 10L)
  expect_equal( as_int(as.raw(10L)), 10L)
  expect_equal( as_int(TRUE)       , 1L )

  expect_equal( as_double(10), 10.0)
  expect_equal( as_double(10L), 10.0)
  expect_equal( as_double(as.raw(10L)), 10.0)
  expect_equal( as_double(TRUE), 1.0)

  expect_equal( as_raw(10), as.raw(10))
  expect_equal( as_raw(10L), as.raw(10))
  expect_equal( as_raw(as.raw(10L)), as.raw(10))
  expect_equal( as_raw(TRUE), as.raw(1))
  
  expect_equal( as_bool(10), as.logical(10) )
  expect_equal( as_bool(10L), as.logical(10) )
  expect_equal( as_bool(as.raw(10L)), as.logical(10) )
  expect_equal( as_bool(TRUE), as.logical(1) )
  
  expect_equal( as_string("foo"), "foo" )
})

test_that( "as handles std:::vector of various types", {
  expect_equal( as_vector_int(1:10), 1:10 )
  expect_equal( as_vector_int(as.numeric(1:10)), 1:10 )
  expect_equal( as_vector_int(as.raw(1:10)), 1:10 )
  expect_equal( as_vector_int(c(TRUE,FALSE)), 1:0 )
  
  expect_equal( as_vector_double(1:10), as.numeric(1:10) )
  expect_equal( as_vector_double(as.numeric(1:10)), as.numeric(1:10) )
  expect_equal( as_vector_double(as.raw(1:10)), as.numeric(1:10))
  expect_equal( as_vector_double(c(TRUE,FALSE)), c(1.0, 0.0) )
  
  expect_equal( as_vector_raw(1:10), as.raw(1:10) )
  expect_equal( as_vector_raw(as.numeric(1:10)), as.raw(1:10) )
  expect_equal( as_vector_raw(as.raw(1:10)), as.raw(1:10) )
  expect_equal( as_vector_raw(c(TRUE,FALSE)), as.raw(1:0) )
  
  expect_equal( as_vector_bool(0:10), as.logical(0:10) )
  expect_equal( as_vector_bool(as.numeric(0:10)), as.logical(0:10) )
  expect_equal( as_vector_bool(as.raw(0:10)), as.logical(0:10) )
  expect_equal( as_vector_bool(c(TRUE,FALSE)), as.logical(1:0) )
  
  expect_equal( as_vector_string(letters), letters )
})

test_that( "as handles std::deque", {
  expect_equal( as_deque_int(1:10), 1:10 )
})

test_that( "as handles std::list", {
  expect_equal( as_list_int(1:10), 1:10 )
})

