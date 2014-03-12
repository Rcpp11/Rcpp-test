context( "Matrix" )
# sourceCpp( "cpp/Matrix.cpp" )

# test_that( ".row and .column works in several places", {
# 	x <- matrix( 1:16+.5, nc = 4 )
# 	res <- runit_Row_Column_sugar( x )
# 	target <- list(
# 	    x[1,],
# 	    x[,1],
# 	    x[2,],
# 	    x[,2],
# 	    x[2,] + x[,2]
# 	    )
# 	expect_equal( res, target )
# 
# 	x <- matrix( 1:16 + .5, ncol = 4 )
# 	expect_equal( runit_NumericMatrix_row( x ), sum( x[1,] ))
# 
# 	m <- matrix( letters, ncol = 2 )
# 	expect_equal( runit_CharacterMatrix_row(m), paste( m[1,], collapse = "" ))
# 
# 	m <- lapply( 1:16, function(i) seq(from=1, to = i ) )
# 	dim( m ) <- c( 4, 4 )
# 	expect_equal( runit_GenericMatrix_row( m ), 1 + 0:3*4)
# 
# 	x <- matrix( 1:16 + .5, ncol = 4 )
# 	expect_equal( runit_NumericMatrix_column( x ), sum( x[,1] ) )
# })
# 
# 
# test_that( "Matrix handles primitive types", {
# 	x <- matrix( 1:16 + .5, ncol = 4 )
# 	expect_equal( matrix_numeric(x), sum(diag(x)))
# 
# 	y <- as.vector( x )
# 	expect_error( matrix_numeric(y) )
# 
# 	x <- matrix( letters[1:16], ncol = 4 )
# 	expect_equal( matrix_character(x), paste( diag(x), collapse = "" ) )
# 
# 	g <- function(y){
# 		sapply( y, function(x) seq(from=x, to = 16) )
# 	}
# 	x <- matrix( g(1:16), ncol = 4 )
# 	expect_equal( matrix_generic(x), g(diag(matrix(1:16,ncol=4))))
# 
# })
# 
# test_that( "Matrix creates a matrix from a number of rows and columns", {
# 	x <- matrix(0, 3, 3)
# 	expect_equal( matrix_numeric_ctor1(), x)
# 	expect_equal( matrix_numeric_ctor2(), x)
# })
# 
# test_that( "Matrix handles indexing", {
#   x <- matrix( 1:16, ncol = 4 )
#   expect_equal( integer_matrix_indexing(x), sum(diag(x)))
#   
#   expect_equal( diag(integer_matrix_indexing_lhs(x)), 2*0:3)
#   
#   y <- as.vector( x )
#   expect_error( integer_matrix_indexing_lhs(y) )
# })
# 
# test_that( "Matrix columns work", {
# 	x <- matrix( 1:8 + .5, ncol = 2 )
# 	expect_equal( runit_NumericMatrix_cumsum( x ), t(apply(x, 1, cumsum)) )
# 
# 	m <- matrix( letters, ncol = 2 )
# 	expect_equal( runit_CharacterMatrix_column(m), paste( m[,1], collapse = "" ))
# 
# 	m <- lapply( 1:16, function(i) seq(from=1, to = i ) )
# 	dim( m ) <- c( 4, 4 )
# 	expect_equal( runit_GenericMatrix_column( m ), 1:4)
# 
# 	probs <- matrix(1:12,nrow=3)
#     expect_equal( runit_NumericMatrix_colsum( probs ), t(apply(probs,1,cumsum)) )
# } )
# 
# test_that( "Matrix rows work", {
#     probs <- matrix(1:12,nrow=3)
#     expect_equal( runit_NumericMatrix_rowsum( probs ), apply(probs,2,cumsum) )
# })
# 
# test_that( "matrix indexing works", {
#     x <- matrix( as.character(1:16), ncol = 4 )
#     expect_equal( character_matrix_indexing(x), paste(diag(x), collapse = ""))
# 
#     y <- as.vector( x )
#     expect_error( fun(y) )
# 
#     expect_equal( diag(character_matrix_indexing_lhs(x)), rep("foo", 4) )
# })
# 
# test_that( "Matrix::row works", {
#     x <- matrix(letters[1:16], nrow = 4)
# 
#     expect_equal( character_matrix_row_iteration_incr(x), "bfjn")
#     expect_equal( character_matrix_row_iteration_decr(x), "njf")
# })

