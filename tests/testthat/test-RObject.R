context( "RObject" )
sourceCpp( "cpp/RObject.cpp" )

test_that( "implicit as and wrap work fine with attributes", {

	expect_equal( asDouble(2.123), 4.246)
	expect_equal( asDouble(2L), 4.0)
	expect_equal( asDouble(as.raw(2L)), 4.0)
	expect_error( asDouble('2'))
	expect_error( asDouble(2:3))

	expect_equal( asInt(2.123), 4L)
	expect_equal( asInt(2), 4L)
	expect_equal( asInt(2L), 4.0)
	expect_equal( asInt(as.raw(2L)), 4.0)
	expect_error( asInt( '2'))
	expect_error( asInt( 2:3))

	expect_equal( asStdString("abc"), "abcabc")
	expect_error( asStdString(NULL))
	expect_error( asStdString(0L))
	expect_error( asStdString(0.1))
	expect_error( asStdString(as.raw(0L)))

	expect_error( asStdString(letters))

	expect_equal( asRaw(1L), as.raw(2L))
	expect_equal( asRaw(1.3), as.raw(2L))
	expect_equal( asRaw(as.raw(1)), as.raw(2L))
	expect_error( asRaw(NULL) )
	expect_error( asRaw("foo") )
	expect_error( asRaw(1:2))
	expect_error( asRaw(as.numeric(1:2)))
	expect_error( asRaw(as.raw(1:3)))
	expect_error( asRaw(integer(0)))
	expect_error( asRaw(numeric(0)))
	expect_error( asRaw(raw(0)))

	expect_true( !asLogical(TRUE))
	expect_true( asLogical(FALSE))
	expect_true( !asLogical(1L))
	expect_true( asLogical(0L))
	expect_true( !asLogical(1.0))
	expect_true( asLogical(0.0))
	expect_true( !asLogical(as.raw(1)))
	expect_true( asLogical(as.raw(0)))

	expect_error( asLogical(NULL))
	expect_error( asLogical(c(TRUE,FALSE)))
	expect_error( asLogical(1:2))
	expect_error( asLogical(1:2+.1))
	expect_error( asLogical(as.raw(1:2)))

	expect_error( asLogical(integer(0)))
	expect_error( asLogical(numeric(0)))
	expect_error( asLogical(raw(0)))

	expect_equal( asStdVectorInt(x=2:5), 2:5*2L)
  expect_equal( asStdVectorInt(x=2:5+.1), 2:5*2L)
  expect_equal( asStdVectorInt(x=as.raw(2:5)), 2:5*2L)
  expect_error( asStdVectorInt("foo"))
  expect_error( asStdVectorInt(NULL))

  expect_equal( asStdVectorDouble(x=0.1+2:5), 2*(0.1+2:5))
  expect_equal( asStdVectorDouble(x=2:5), 2*(2:5))
  expect_equal( asStdVectorDouble(x=as.raw(2:5)), 2*(2:5))
  expect_error( asStdVectorDouble("foo"))
  expect_error( asStdVectorDouble(NULL))

	expect_equal( asStdVectorRaw(x=as.raw(0:9)), as.raw(2*(0:9)))
  expect_equal( asStdVectorRaw(x=0:9), as.raw(2*(0:9)))
  expect_equal( asStdVectorRaw(x=as.numeric(0:9)), as.raw(2*(0:9)))
  expect_error( asStdVectorRaw("foo"))
  expect_error( asStdVectorRaw(NULL))

	expect_equal( asStdVectorBool(x=c(TRUE,FALSE)), c(FALSE, TRUE))
  expect_equal( asStdVectorBool(x=c(1L, 0L)), c(FALSE, TRUE))
  expect_equal( asStdVectorBool(x=c(1.0, 0.0)), c(FALSE, TRUE))
  expect_equal( asStdVectorBool(x=as.raw(c(1,0))), c(FALSE, TRUE))
  expect_error( asStdVectorBool("foo"))
  expect_error( asStdVectorBool(NULL))

	expect_equal( asStdVectorString(c("foo", "bar")), c("foofoo", "barbar"))
	expect_error( asStdVectorString(1L))
	expect_error( asStdVectorString(1.0))
	expect_error( asStdVectorString(as.raw(1)))
	expect_error( asStdVectorString(TRUE))
	expect_error( asStdVectorString(NULL))

	expect_equal( stdsetint(), c(0L, 1L))

	expect_equal( stdsetdouble(), as.numeric(0:1))

	expect_equal( stdsetraw(), as.raw(0:1))
	expect_equal( stdsetstring(), c("bar", "foo"))
})

test_that( "api classes handle attributes", {
	df <- data.frame( x = 1:10, y = 1:10 )
	expect_true( all( c("names","row.names","class") %in% attributeNames(df)))

	df <- data.frame( x = 1:10 )
	expect_true( hasAttribute( df ))

	df <- data.frame( x = 1:150 )
	rownames(df) <- 1:150
	expect_equal( attr_( iris ), 1:150)

	expect_equal( attr(attr_set(), "foo"), 10L)
})

test_that( ".isNULL works as expected", {
	df <- data.frame( x = 1:10 )
	expect_true( !isNULL( df ))
	expect_true( !isNULL(1L))
	expect_true( !isNULL(1.0))
	expect_true( !isNULL(as.raw(1)))
	expect_true( !isNULL(letters))
	expect_true( !isNULL(.GlobalEnv))
	expect_true( isNULL(NULL))
})

test_that( ".inherits works", {
	x <- 1:10
	expect_true( !inherits_(x) )
	class(x) <- "foo"
	expect_true( inherits_(x) )
	class(x) <- c("foo", "bar" )
	expect_true( inherits_(x) )
})

