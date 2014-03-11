context( "String" )
sourceCpp( "cpp/String.cpp" )

test_that( "replace functions work", {
  expect_equal( String_replace_all("foobar", "o", "*"), "f**bar")
  expect_equal( String_replace_first("foobar", "o", "*"), "f*obar")
  expect_equal( String_replace_last("foobar", "o", "*"), "fo*bar")

  res <- test_sapply_string( "foobar", c("o", "a" ), c("*", "!" ) )
  expect_equal( res, "f**b!r" )    
})

test_that( "String comparison works", {
  res <- test_compare_Strings( "aaa", "aab" )
  target <- list( 
    "a  < b" = TRUE, 
    "a  > b" = FALSE,  
    "a == b" = FALSE,
    "a == a" = TRUE
  )
  expect_equal( res, target )
})
             
test_that( "wstring are supported", {
  expect_equal( CharacterVector_wstring(), c("foobar", "foobar" ) )
  expect_equal( wstring_return(), "foo" )
  expect_equal( wstring_param( "foo", "bar" ), "foobar" )
  expect_equal( wrap_vector_wstring( ), c("foo", "bar" ) )
})
