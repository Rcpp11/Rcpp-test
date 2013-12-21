context( "wstring" )
sourceCpp( "cpp/wstring.cpp", env = environment()

test_that( "wstring are supported", {
  expect_equal( CharacterVector_wstring(), c("foobar", "foobar" ) )
  expect_equal( wstring_return(), "foo" )
  expect_equal( wstring_param( "foo", "bar" ), "foobar" )
  expect_equal( wrap_vector_wstring( ), c("foo", "bar" ) )
})
