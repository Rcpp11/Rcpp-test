context( "Matrix" )
sourceCpp( "cpp/Matrix.cpp", env = environment() )

test_that( ".row and .column works in several places", {
	x <- matrix( 1:16+.5, nc = 4 )
	res <- runit_Row_Column_sugar( x )
	target <- list(
	    x[1,],
	    x[,1],
	    x[2,],
	    x[,2],
	    x[2,] + x[,2]
	    )
	expect_equal( res, target )

	x <- matrix( 1:16 + .5, ncol = 4 )
	expect_equal( runit_NumericMatrix_row( x ), sum( x[1,] ))

	m <- matrix( letters, ncol = 2 )
	expect_equal( runit_CharacterMatrix_row(m), paste( m[1,], collapse = "" ))

	m <- lapply( 1:16, function(i) seq(from=1, to = i ) )
	dim( m ) <- c( 4, 4 )
	expect_equal( runit_GenericMatrix_row( m ), 1 + 0:3*4)

	x <- matrix( 1:16 + .5, ncol = 4 )
	expect_equal( runit_NumericMatrix_column( x ), sum( x[,1] ) )
})


test_that( "Matrix handles primitive types", {
	x <- matrix( 1:16 + .5, ncol = 4 )
	expect_equal( matrix_numeric(x), sum(diag(x)))

	y <- as.vector( x )
	expect_error( matrix_numeric(y) )

	x <- matrix( letters[1:16], ncol = 4 )
	expect_equal( matrix_character(x), paste( diag(x), collapse = "" ) )

	g <- function(y){
		sapply( y, function(x) seq(from=x, to = 16) )
	}
	x <- matrix( g(1:16), ncol = 4 )
	expect_equal( matrix_generic(x), g(diag(matrix(1:16,ncol=4))))

} )

test_that( "Matrix:diag works", {
	expected <- matrix( 0L, nrow = 5, ncol = 5 )
	diag( expected ) <- 1L
	expect_equal( matrix_integer_diag(), expected)

	expected <- matrix( "", nrow = 5, ncol = 5 )
	diag( expected ) <- "foo"
	expect_equal( matrix_character_diag(), expected)
})

test_that( "Matrix creates a matrix from a number of rows and columns", {
	x <- matrix(0, 3, 3)
	expect_equal( matrix_numeric_ctor1(), x)
	expect_equal( matrix_numeric_ctor2(), x)
})

test_that( "Matrix handles indexing", {
  x <- matrix( 1:16, ncol = 4 )
  expect_equal( integer_matrix_indexing(x), sum(diag(x)))
  
  expect_equal( diag(integer_matrix_indexing_lhs(x)), 2*0:3)
  
  y <- as.vector( x )
  expect_error( integer_matrix_indexing_lhs(y) )
})

test_that( "Matrix columns work", {
	x <- matrix( 1:8 + .5, ncol = 2 )
	expect_equal( runit_NumericMatrix_cumsum( x ), t(apply(x, 1, cumsum)) )

	m <- matrix( letters, ncol = 2 )
	expect_equal( runit_CharacterMatrix_column(m), paste( m[,1], collapse = "" ))

	m <- lapply( 1:16, function(i) seq(from=1, to = i ) )
	dim( m ) <- c( 4, 4 )
	expect_equal( runit_GenericMatrix_column( m ), 1:4)

	probs <- matrix(1:12,nrow=3)
    expect_equal( runit_NumericMatrix_colsum( probs ), t(apply(probs,1,cumsum)) )
} )

test_that( "Matrix rows work", {
    probs <- matrix(1:12,nrow=3)
    expect_equal( runit_NumericMatrix_rowsum( probs ), apply(probs,2,cumsum) )
})

test_that( "SubMatrix works", {
    target <- rbind( c(3,4,5,5), c(3,4,5,5), 0 )
    expect_equal( runit_SubMatrix(), target)
})

