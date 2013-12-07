context( "Language" )
sourceCpp( "cpp/Language.cpp" )

test_that( "Language handles R objects", {
	expect_equal( runit_language( call("rnorm") ), call("rnorm" ))
	expect_error( runit_language(test.Language))
	expect_error( runit_language(new.env()))
	expect_error( runit_language(1:10))
	expect_error( runit_language(TRUE))
	expect_error( runit_language(1.3))
	expect_error( runit_language(as.raw(1) ))
})

test_that( "Languge handles arbitrary number of arguments", {
	expect_equal( runit_lang_variadic_1(), call("rnorm", 10L, 0.0, 2.0 ) )
	expect_equal( runit_lang_variadic_2(), call("rnorm", 10L, mean = 0.0, 2.0 ) )
})

test_that( "Language::push_back works", {
	expect_equal( runit_lang_push_back(), call("rnorm", 10L, mean = 0.0, 2.0 ) )
})

test_that( "Language handles index operator", {
	expect_equal( runit_lang_square_rv(), 10.0)
	expect_equal( runit_lang_square_lv(), call("rnorm", "foobar", 20.0, 20.0) )
})

test_that( "Language can evaluate", {
	expect_equal( runit_lang_fun(sort, sample(1:10)), 1:10)
	e <- new.env()
	e[["y"]] <- 1:10
	expect_equal( runit_lang_inenv(e), sum(1:10))
})

test_that( "Language can be wraped as unary_call, fixed_call, and binary_call", {
	expect_equal(
		runit_lang_unarycall( 1:10 ),
		lapply( 1:10, function(n) seq(from=n, to = 0 ) ) 
	)
	expect_equal(
		runit_lang_unarycallindex( 1:10 ),
		lapply( 1:10, function(n) seq(from=10, to = n )) )
	expect_equal(
		runit_lang_binarycall( 1:10, 11:20 ),
		lapply( 1:10, function(n) seq(n, n+10) ) )
	set.seed(123)
	res <- runit_lang_fixedcall()
	set.seed(123)
	exp <- lapply( 1:10, function(n) rnorm(10) )
	expect_equal( res, exp)
})

context( "Pairlist" )

test_that( "Pairlist handles R objects", {
	expect_equal( runit_pairlist( pairlist("rnorm") ), pairlist("rnorm" ))
	expect_equal( runit_pairlist( call("rnorm") ), pairlist(as.name("rnorm")))
	expect_equal( runit_pairlist(1:10), as.pairlist(1:10) )
	expect_equal( runit_pairlist(TRUE), as.pairlist( TRUE) )
	expect_equal( runit_pairlist(1.3), as.pairlist(1.3))
	expect_equal( runit_pairlist(as.raw(1) ), as.pairlist(as.raw(1)))

	expect_error( runit_pairlist(runit_pairlist))
	expect_error( runit_pairlist(new.env()))

})

test_that( "Pairlist can handle arbitrary number of arguments", {
	expect_equal( runit_pl_variadic_1(), pairlist("rnorm", 10L, 0.0, 2.0 ),
		msg = "variadic templates" )
	expect_equal( runit_pl_variadic_2(), pairlist("rnorm", 10L, mean = 0.0, 2.0 ),
		msg = "variadic templates (with names)" )
})

test_that( "Pairlist has stl-type methods: size, remove, insert, replace, push_front and push_back", {
	expect_equal( runit_pl_push_front(),
		pairlist( foobar = 10, "foo", 10.0, 1L))
	expect_equal( runit_pl_push_back(),
		pairlist( 1L, 10.0, "foo", foobar = 10))
	expect_equal( runit_pl_insert(),
		pairlist( 30.0, 1L, bla = "bla", 10.0, 20.0, "foobar" ))
	expect_equal( runit_pl_replace(),
		pairlist( first = 1, 20.0 , FALSE))
	expect_equal( runit_pl_size(), 3L)
	expect_equal( runit_pl_remove_1(), pairlist(10.0, 20.0))
	expect_equal( runit_pl_remove_2(), pairlist(1L, 10.0))
	expect_equal( runit_pl_remove_3(), pairlist(1L, 20.0))
})

test_that( "Pairlist::operator[]", {
	expect_equal( runit_pl_square_1(), 10.0)
	expect_equal( runit_pl_square_2(), pairlist(1L, "foobar", 1L) )
})

context( "Formula" )

test_that( "Formula works", {
	expect_equal( runit_formula_(), x ~ y + z)

	expect_equal( runit_formula_SEXP( x ~ y + z), x ~ y + z)
	expect_equal( runit_formula_SEXP( "x ~ y + z" ), x ~ y + z)
	expect_equal( runit_formula_SEXP( parse( text = "x ~ y + z") ), x ~ y + z)
	expect_equal( runit_formula_SEXP( list( "x ~ y + z") ), x ~ y + z)
	expect_equal( runit_formula_SEXP( list( x ~ y + z) ), x ~ y + z)
})

