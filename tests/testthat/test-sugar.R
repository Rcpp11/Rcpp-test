context( "sugar" )


test_that( "abs handles numeric and integer",{
	x <- rnorm(10)
	y <- -10:10
	expect_equal( runit_abs(x,y) , list( abs(x), abs(y) ) )
})

# test_that( "all works on several expressions", {
# 	expect_true( runit_all_one_less( 1 ) )
# 	expect_true( ! runit_all_one_less( 1:10 ) )
# 	expect_true( is.na( runit_all_one_less( NA ) ) )
# 	expect_true( is.na( runit_all_one_less( c( NA, 1)  ) ) )
# 	expect_true( ! runit_all_one_less( c( 6, NA)  ) )
# 
# 	expect_true( ! runit_all_one_greater( 1 ) )
# 	expect_true( ! runit_all_one_greater( 1:10 ) )
# 	expect_true( runit_all_one_greater( 6:10 ) )
# 	expect_true( ! runit_all_one_greater( c(NA, 1) ) )
# 	expect_true( is.na( runit_all_one_greater( c(NA, 6) ) ) )
# 
# 	expect_true( runit_all_one_less_or_equal( 1 ) )
# 	expect_true( ! runit_all_one_less_or_equal( 1:10 ) )
# 	expect_true( is.na( runit_all_one_less_or_equal( NA ) ) )
# 	expect_true( is.na( runit_all_one_less_or_equal( c( NA, 1)  ) ) )
# 	expect_true( ! runit_all_one_less_or_equal( c( 6, NA)  ) )
# 	expect_true( runit_all_one_less_or_equal( 5 ) )
# 
# 	expect_true( ! runit_all_one_greater_or_equal( 1 ) )
# 	expect_true( ! runit_all_one_greater_or_equal( 1:10 ) )
# 	expect_true( runit_all_one_greater_or_equal( 6:10 ) )
# 	expect_true( runit_all_one_greater_or_equal( 5 ) )
# 	expect_true( ! runit_all_one_greater_or_equal( c(NA, 1) ) )
# 	expect_true( is.na( runit_all_one_greater_or_equal( c(NA, 6) ) ) )
# 
# 	expect_true( ! runit_all_one_equal( 1 ) )
# 	expect_true( ! runit_all_one_equal( 1:2 ) )
# 	expect_true( runit_all_one_equal( rep(5,4) ) )
# 	expect_true( is.na( runit_all_one_equal( c(5,NA) ) ) )
# 	expect_true(! runit_all_one_equal( c(NA, 1) ) )
# 
# 	expect_true( runit_all_not_equal_one( 1 ) )
# 	expect_true( runit_all_not_equal_one( 1:2 ) )
# 	expect_true( ! runit_all_not_equal_one( 5 ) )
# 	expect_true( is.na( runit_all_not_equal_one( c(NA, 1) ) ) )
# 	expect_true( ! runit_all_not_equal_one( c(NA, 5) ) )
# 	
# 	expect_true( ! runit_all_less( 1, 0 ) )
# 	expect_true( runit_all_less( 1:10, 2:11 ) )
# 	expect_true( runit_all_less( 0, 1 ) )
# 	expect_true( is.na( runit_all_less( NA, 1 ) ) )
# 
# 	expect_true( runit_all_greater( 1, 0 ) )
# 	expect_true( runit_all_greater( 2:11, 1:10 ) )
# 	expect_true( ! runit_all_greater( 0, 1 ) )
# 	expect_true( ! runit_all_greater( 0:9, c(0:8,10) ) )
# 	expect_true( is.na( runit_all_greater( NA, 1 ) ) )
# 
# 	expect_true( runit_all_less_or_equal( 1, 1 ) )
# 	expect_true( ! runit_all_less_or_equal( 1:2, c(1,1) ) )
# 	expect_true( runit_all_less_or_equal( 0, 1 ) )
# 	expect_true( ! runit_all_less_or_equal( 1, 0 ) )
# 	expect_true( is.na( runit_all_less_or_equal( NA, 1 ) ) )
# 
# 	expect_true( runit_all_greater_or_equal( 1, 1 ) )
# 	expect_true( runit_all_greater_or_equal( 1:2, c(1,1) ) )
# 	expect_true( ! runit_all_greater_or_equal( 0, 1 ) )
# 	expect_true( runit_all_greater_or_equal( 1, 0 ) )
# 	expect_true( is.na( runit_all_greater_or_equal( NA, 1 ) ) )
# 
# 	expect_true( runit_all_equal( 1, 1 ) )
# 	expect_true( ! runit_all_equal( 1:2, c(1,1) ) )
# 	expect_true( ! runit_all_equal( 0, 1 ) )
# 	expect_true( ! runit_all_equal( 1, 0 ) )
# 	expect_true( is.na( runit_all_equal( NA, 1 ) ) )
# 
# 	expect_true( ! runit_all_not_equal( 1, 1 ) )
# 	expect_true( ! runit_all_not_equal( 1:2, c(1,1) ) )
# 	expect_true( runit_all_not_equal( 0, 1 ) )
# 	expect_true( runit_all_not_equal( 1, 0 ) )
# 	expect_true( is.na( runit_all_not_equal( NA, 1 ) ) )
# 
# } )
# 
# test_that( "any works with various expressions", { 
# 	expect_true( ! runit_any_less( 1, 0 ) )
# 	expect_true( runit_any_less( 1:10, 2:11 ) )
# 	expect_true( runit_any_less( 0, 1 ) )
# 	expect_true( is.na( runit_any_less( NA, 1 ) ) )
# 
# 	expect_true( runit_any_greater( 1, 0 ) )
# 	expect_true( runit_any_greater( 2:11, 1:10 ) )
# 	expect_true( ! runit_any_greater( 0, 1 ) )
# 	expect_true( is.na( runit_any_greater( NA, 1 ) ) )
# 
# 	expect_true( runit_any_less_or_equal( 1, 1 ) )
# 	expect_true( runit_any_less_or_equal( 1:2, c(1,1) ) )
# 	expect_true( runit_any_less_or_equal( 0, 1 ) )
# 	expect_true( ! runit_any_less_or_equal( 1, 0 ) )
# 	expect_true( is.na( runit_any_less_or_equal( NA, 1 ) ) )
# 
# 	expect_true( runit_any_greater_or_equal( 1, 1 ) )
# 	expect_true( runit_any_greater_or_equal( 1:2, c(1,1) ) )
# 	expect_true( ! runit_any_greater_or_equal( 0, 1 ) )
# 	expect_true( runit_any_greater_or_equal( 1, 0 ) )
# 	expect_true( is.na( runit_any_greater_or_equal( NA, 1 ) ) )
# 
# 	expect_true( runit_any_equal( 1, 1 ) )
# 	expect_true( runit_any_equal( 1:2, c(1,1) ) )
# 	expect_true( ! runit_any_equal( 0, 1 ) )
# 	expect_true( ! runit_any_equal( 1, 0 ) )
# 	expect_true( is.na( runit_any_equal( NA, 1 ) ) )
# 
# 	expect_true( ! runit_any_not_equal( 1, 1 ) )
# 	expect_true( runit_any_not_equal( 1:2, c(1,1) ) )
# 	expect_true( runit_any_not_equal( 0, 1 ) )
# 	expect_true( runit_any_not_equal( 1, 0 ) )
# 	expect_true( is.na( runit_any_not_equal( NA, 1 ) ) )
# 	
# 	expect_equal( runit_any_isna( c(1:5,NA,7:10) ) , TRUE )
# 	
# 	expect_true( ! runit_any_equal_not( 1, 1 ) )
# 	expect_true( runit_any_equal_not( 1:2, c(1,1) ) )
# 	expect_true( runit_any_equal_not( 0, 1 ) )
# 	expect_true( runit_any_equal_not( 1, 0 ) )
# 	expect_true( is.na( runit_any_equal_not( NA, 1 ) ) )
# })

test_that( "LogicalVector can be contructed and assigned to sugar expressions", {
	expect_equal( runit_constructor( 1, 0 ), FALSE )
	expect_equal( runit_constructor( 1:10, 2:11 ), rep(TRUE,10) )
	expect_equal( runit_constructor( 0, 1 ), TRUE )
	expect_true( identical( runit_constructor( NA, 1 ), NA ) )

	expect_equal( runit_assignment( 1, 0 ), FALSE )
	expect_equal( runit_assignment( 1:10, 2:11 ), rep(TRUE,10) )
	expect_equal( runit_assignment( 0, 1 ), TRUE )
	expect_true( identical( runit_assignment( NA, 1 ), NA ) )
})

test_that( "sugar diff works", {
  x <- as.integer(round(rnorm(100,1,100)))
  expect_equal( runit_diff_int(x) , diff(x) )

  x <- rnorm( 100 )
  expect_equal( runit_diff(x) , diff(x) )

  y    <- rnorm(100)
  pred <- sample( c(T,F), 99, replace = TRUE )
  expect_equal( runit_diff_ifelse(pred, x, y ), ifelse( pred, diff(x), diff(y) ) )

  x <- c( NA, 1.5, 2.5, NA, 3.5, 5.5, NA )
  expect_equal( runit_diff_REALSXP_NA(x), c(NA, 1.0, NA, NA, 2.0, NA) )
})

test_that( "sugar math functions work", {
  x <- rnorm(10)
  y <- -10:10
  expect_equal( runit_exp(x,y) , list( exp(x), exp(y) ) )
  expect_equal( runit_floor(x,y) , list( floor(x), floor(y) ) )
  expect_equal( runit_ceil(x,y) , list( ceiling(x), ceiling(y) ) )
	expect_equal( runit_pow(x,y) , list( x^3L , y^2.3 ) )
})

test_that( "ifelse works", {
	x <- 1:10
	y <- 10:1
	expect_equal( runit_ifelse( x, y), list(
		"vec_vec"   = ifelse( x<y, x*x, -(y*y) ) ,
		"vec_prim"  = ifelse( x<y, 1.0, -(y*y) ),
		"prim_vec"  = ifelse( x<y, x*x, 1.0    ),
		"prim_prim" = ifelse( x<y, 1.0, 2.0    )
	))
})

# test_that( "sugar handles is_na, is_finite", { 
# 	expect_equal( runit_isna( 1:10) , rep(FALSE,10) )
# 	expect_equal(
# 	    runit_isfinite( c(1, NA, Inf, -Inf, NaN) ) ,
# 	    c(TRUE, FALSE, FALSE, FALSE, FALSE)
# 	)
# 	expect_equal(
# 	    runit_isinfinite( c(1, NA, Inf, -Inf, NaN) ) ,
# 	    c(FALSE, FALSE, TRUE, TRUE, FALSE)
# 	)
# 	expect_equal(
# 	    runit_isnan( c(1, NA, Inf, -Inf, NaN) ) ,
# 	    c(FALSE, FALSE, FALSE, FALSE, TRUE)
# 	)
# 	expect_equal( runit_isna_isna( c(1:5,NA,7:10) ) , rep(FALSE,10) )
# })

test_that( "(sl)apply works", { 
	expect_equal( runit_lapply( 1:10 ), lapply( 1:10, seq_len ) )
	expect_equal( runit_sapply(1:10) , (1:10)^2 )
	expect_equal( runit_sapply_rawfun(1:10) , (1:10)^2 )
	# expect_true( ! runit_sapply_square(1:10)  )
	expect_equal( runit_sapply_list(1:10), lapply( 1:10, seq_len ) )
})

test_that( "unary and binary arithmetic operations work", { 
	expect_equal(
	    runit_minus(1:10) ,
	    list( (1:10)-10L, 10L-(1:10), rep(0L,10), (1:10)-10L, 10L-(1:10)  )
	    )
	expect_equal( runit_plus(1:10) , list( 11:20,11:20,1:10+1:10, 3*(1:10))  )
	expect_equal( runit_plus_seqlen() , list( 11:20,11:20, 1:10+1:10)  )
	# expect_equal( runit_plus_all(1:10) , FALSE )
	expect_equal( runit_times(1:10) ,
		list(
			10L*(1:10),
			10L*(1:10),
			(1:10)*(1:10),
			(1:10)*(1:10)*(1:10),
			c(NA,(2:10)*(2:10)),
			c(NA,10L*(2:10)),
			c(NA,10L*(2:10)),
			rep( NA_integer_, 10L )
		)
	)
	expect_equal( runit_divides(1:10) ,
		list(
			1:10/10,
			10/1:10,
			rep(1,10)
		)
	)
	expect_equal( runit_unary_minus( seq(0,5,by=10) ), - seq(0,5,by=10) )
	expect_true( identical( runit_unary_minus( c(0,NA,2) ), c(0,NA,-2) ) )
	
	x <- rnorm(10)
  expect_equal(vector_scalar_ops(x), list(x + 2, 2 - x, x * 2, 2 / x))
  
  x <- rnorm(10) + 2
  expect_equal(vector_scalar_logical(x), list(x < 2, 2 > x, x <= 2, 2 != x) )
  
  x <- rnorm(10)
  y <- runif(10)
  expect_equal(vector_vector_ops(x,y), list(x + y, y - x, x * y, y / x))
  expect_equal(vector_vector_logical(x,y), list(x < y, x > y, x <= y, x >= y, x == y, x != y))
  
  x <- (1+1i) * 1:10
  y <- (2-3i) * 1:10
  
  expect_equal(
      complex_binary_sugar(x, y),
      list(
          "+" = x + y,
          "-" = x - y,
          "*" = x * y,
          "/" = x / y
          )) 
  
})

test_that( "pmin and pmax work", {
	expect_equal( runit_pmin(1:10, 10:1) , c(1:5,5:1) )
	expect_equal( runit_pmin_one(1:10) ,
		list(
			c(1:5,rep(5,5)),
			c(1:5,rep(5,5))
		)
	)
	expect_equal( runit_pmax(1:10, 10:1) , c(10:6,6:10) )
	expect_equal( runit_pmax_one(1:10) ,
		list(
			c(rep(5,5), 6:10),
			c(rep(5,5), 6:10)
		)
	) 
})

test_that( "seq_len and seq_along work", {
	expect_equal( runit_seqalong( rnorm(10)) , 1:10  )
	expect_equal( runit_seqlen() , 1:10  )
})

test_that( "sign works", {
	expect_equal(
		runit_sign( seq(-10, 10, length.out = 51), -25:25 ),
		list(
			c( rep(-1L, 25), 0L, rep(1L, 25) ),
			c( rep(-1L, 25), 0L, rep(1L, 25) )
		)
	)
})

test_that( "implicit wrap handles sugar expressions", {
	e <- new.env()
	runit_wrap( 1:10, 2:11, e )
	expect_equal( e[["foo"]], rep(TRUE, 10 ) )
})

test_that( "complex are correctly handled", {
	x <- c( rnorm(10), NA ) + 1i*c( rnorm(10), NA )
	expect_equal( runit_complex(x), list(
		Re    = Re(x),
		Im    = Im(x),
		Conj  = Conj(x),
		Mod   = Mod(x),
		exp   = exp(x),
		log   = log(x),
		sqrt  = sqrt(x),
		cos   = cos(x),
		sin   = sin(x),
		tan   = tan(x),
		acos  = acos(x),
		asin  = asin(x),
		atan  = atan(x),
		# acosh = acosh(x),
		asinh = asinh(x),
		atanh = atanh(x),
		cosh  = cosh(x),
		sinh = sinh(x),
		tanh = tanh(x)
		)
	)
})

test_that( "rep works", {
	expect_equal( runit_rep(1:10),
		list(
			"rep" = rep( 1:10, 3 ),
			"rep_each" = rep( 1:10, each = 3 ),
			"rep_len" = rep( 1:10, length.out = 12 ),
			"rep_prim_double" = rep( 0.0, 10 )
		)
	)
})

test_that( "rev works", {
	expect_equal( runit_rev(1:10), rev( 1:10 * 1:10 ) )
})

test_that( "head and tail work", {
  expect_equal(
    runit_head(1:100),
    list( pos = 1:5, neg = 1:95 )
  	)
  expect_equal(
    runit_tail(1:100),
    list( pos = 96:100, neg = 6:100 )
  	)
})

context( "sugar matrix")

test_that( "outer works", {
	x <- 1:2
	y <- 1:5
	expect_equal( runit_outer(x,y) , outer(x,y,"+") )
})
 
test_that( "row works", {
	m <- matrix( 1:16, nc = 4 )
	res <- runit_row( m )
	target <- list( row = row(m), col = col(m) )
	expect_equal( res, target )
})

test_that( "diag works", {
	x <- 1:4
	m <- matrix( 1:16, nc = 4 )
	res <- runit_diag(x, m)
	target <- list(
		    diag(x),
		    diag(m),
		    diag( outer( x, x, "+" ) )
		)
	expect_equal( res, target )
})

context( "sugar" )

test_that( "sugar automatically generated functions work", {
	x <- seq( 1, 5, by = .5 )
	expect_equal( runit_gamma(x),
	 	list(
	  		"gamma"      = gamma(x),
	  		"lgamma"     = lgamma(x),
	  		"digamma"    = digamma(x),
	  		"trigamma"   = trigamma(x),
	  		"tetragamma" = psigamma(x, 2),
	  		"pentagamma" = psigamma(x, 3),
	  		"factorial"  = factorial(x),
	  		"lfactorial" = lfactorial(x)
	  	)
	)
	x <- 10^-(1+2*1:9)
	expect_equal( runit_log1p(x),
		list( log1p = log1p(x), expm1 = expm1(x) ) 
	)
	expect_equal( runit_choose(10:6,5:1),
		list(
			VV = choose( 10:6, 5:1),
			PV = choose( 10, 5:1 ),
			VP = choose( 10:6, 5 )
		) 
	)
	expect_equal( runit_lchoose(10:6,5:1),
		list(
			VV = lchoose( 10:6, 5:1),
			PV = lchoose( 10, 5:1 ),
			VP = lchoose( 10:6, 5 )
		) 
	)
	expect_equal( runit_beta(10:6,5:1),
		list(
			VV = beta( 10:6, 5:1),
			PV = beta( 10, 5:1 ),
			VP = beta( 10:6, 5 )
		) 
	)
	expect_equal( runit_lbeta(10:6,5:1),
		list(
			VV = lbeta( 10:6, 5:1),
			PV = lbeta( 10, 5:1 ),
			VP = lbeta( 10:6, 5 )
		) )
	expect_equal( runit_psigamma(10:6,5:1),
		list(
			VV = psigamma( 10:6, 5:1),
			PV = psigamma( 10, 5:1 ),
			VP = psigamma( 10:6, 5 )
		) )
})

test_that("sugar sum functions work", {
    x <- rnorm( 10 )
    expect_equal( runit_sum(x), sum(x) )
    x[4] <- NA
    expect_equal( runit_sum(x), sum(x) )

    x <- rnorm( 10 )
    expect_equal( runit_cumsum(x), cumsum(x) )
    x[4] <- NA
    expect_equal( runit_cumsum(x), cumsum(x) )
    
    y <- rnorm(10) + rnorm(10)*1i
    expect_equal( runit_sum_cplx(y), sum(y) )
    expect_equal( runit_mean_cplx(y), mean(y) )
    
})

test_that( "rounding functions work", {
    x <- seq(-5,5) + 0.5
    y <- seq(-5L, 5L)
    expect_equal(runit_trunc(x, y), list(trunc(x), trunc(y)))

    x <- seq(-5,5) + 0.25
    expect_equal( runit_round(x, 0), round(x, 0) )
    expect_equal( runit_round(x, 1), round(x, 1) )
    expect_equal( runit_round(x, 2), round(x, 2) )
    expect_equal( runit_round(x, 3), round(x, 3) )

    x <- seq(-5,5) + 0.25
    expect_equal( runit_signif(x, 0), signif(x, 0) )
    expect_equal( runit_signif(x, 1), signif(x, 1) )
    expect_equal( runit_signif(x, 2), signif(x, 2) )
    expect_equal( runit_signif(x, 3), signif(x, 3) )
})

test_that( "matching functions work", {
    x <- sample( letters, 1000, replace = TRUE )
    expect_true( all( runit_table(x) == table(x) ) )
    expect_true( all( names(runit_table(x)) == names(table(x)) ) )
    expect_equal( runit_duplicated(x), duplicated(x) )
})   

test_that( "set functions work", {
    expect_equal( 
      sort(runit_setdiff( 1:10, 1:5 )), 
      sort(setdiff( 1:10, 1:5))
    )
    expect_equal( 
      sort(runit_union( 1:10, 1:5 )), 
      sort(union( 1:10, 1:5 )) 
    )
    expect_equal( runit_intersect( 1:10, 1:5 ), intersect( 1:10, 1:5 ) )
})

test_that( "clamp works", {
    r_clamp <- function(a, x, b) pmax(a, pmin(x, b) )
    expect_equal(
        runit_clamp( -1, seq(-3,3, length=100), 1 ),
        r_clamp( -1, seq(-3,3, length=100), 1 )
    )
})

