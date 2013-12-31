context( "Rcpp API" )
sourceCpp( "cpp/misc.cpp", env = environment() )

test_that( "Symbol correctly handles various SEXP", {
	res <- symbol_()
	expect_true( res[1L])
	expect_true( res[2L])
	expect_true( res[3L])
	expect_true( res[4L])

	expect_error( symbol_ctor(symbol_ctor))
	expect_error( symbol_ctor(1:10))
	expect_error( symbol_ctor(TRUE))
	expect_error( symbol_ctor(1.3))
	expect_error( symbol_ctor(as.raw(1) ))
})

test_that( "Argument can be used in List::create",{
  expect_equal( Argument_(), list( x = 2L, y = 3L ) )
})

test_that( "Rcpp_eval propagates R errors", {
  expect_error( evaluator_error())
  expect_equal( sort(evaluator_ok(1:10)), 1:10)
})

test_that( "exceptions are correctly mapped to conditions", {
	e <- tryCatch(  exceptions_(), "C++Error" = function(e) e )
	expect_true( "C++Error" %in% class(e))

	expect_true( "std::range_error" %in% class(e))
	expect_equal( e$message, "boom")

	# same with direct handler
	e <- tryCatch(  exceptions_(), "std::range_error" = function(e) e )
	expect_true( "C++Error" %in% class(e))
	expect_true( "std::range_error" %in% class(e))
	expect_equal( e$message, "boom")
	f <- function(){
		try( exceptions_(), silent = TRUE)
		"hello world"
	}
	expect_equal( f(), "hello world")
})

test_that( "has_iterator traits works", {
  has_it <- has_iterator_()
	expect_true( has_it[1L] )
	expect_true( has_it[2L] )
	expect_true( has_it[3L] )
	expect_true( has_it[4L] )
	expect_true( has_it[5L] )

	expect_true( ! has_it[6L] )
	expect_true( ! has_it[7L] )
})

test_that( "Na_Proxy handles comparison to NA", {
    expect_equal( 
        na_proxy(), 
        rep(c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE) , 2)
    )    
})

test_that( "StrechyList correctly builds pairlists", {
    expect_equal( 
      stretchy_list(),
      pairlist( "foo", 1L, 3.2 ) 
    )
    expect_equal(
      named_stretchy_list(),
      pairlist( a = "foo", b = 1L, c = 3.2 )
    )
})

test_that( "Reference works", {
    Instrument <- setRefClass(
       Class="Instrument",
       fields=list("id"="character", "description"="character"), 
       where = environment()
    )
    Instrument$accessors(c("id", "description"))
    
    instrument <- Instrument$new(id="AAPL", description="Apple")
    
    expect_equal( runit_Reference_getId(instrument), "AAPL" )
})

# test_that( "Vector can be constructed from Proxies", {
# 	setClass("track", representation(x="numeric", y="numeric"), where = environment() )
# 	setClass("trackCurve", representation(smooth = "numeric"), contains = "track", where = environment() )
# 
# 	tr1 <- new( "track", x = 2, y = 3 )
# 	expect_equal( S4_get_slot_x(tr1), 2 )
# 
# 	x <- 1:10
# 	attr( x, "foo" ) <- "bar"
# 
# 	expect_equal( S4_get_attr_x(x), "bar" )
# 
# })

test_that( "NA is correctly handled", {
    expect_equal(
        plus_REALSXP(),
        list(NA_real_,NA_real_,NA_real_)
        )
    expect_equal(
        times_REALSXP(),
        list(NA_real_,NA_real_,NA_real_)
        )
    expect_equal(
        divides_REALSXP(),
        list(NA_real_,NA_real_,NA_real_)
        )
    expect_equal(
        minus_REALSXP(),
        list(NA_real_,NA_real_,NA_real_)
        )
    expect_equal(
        functions_REALSXP(),
        list( rep(NA_real_, 20L), rep(NA_real_, 6L) )
        )
})

test_that( "... is correctly handled", {
  res <- countArgs(10, "foo", rfzegvze(), ffz(dsfz(fzfz)) )
  expect_equal(res, 4L)
  
  res <- countNamedArgs(a=1, b=3, c="foo", d = fdzfz() )
  expect_equal(res$count, 4L)
  expect_equal(res$names, letters[1:4])
  
})

test_that( "void can be used as function parameter", {
  expect_equal( void_fun(), 2L )
})

