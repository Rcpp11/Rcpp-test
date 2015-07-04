context( "environments" )


test_that( "Environment::ls() gives correct results", {
	e <- new.env( )
	e$a <- 1:10
	e$b <- "foo"
	e$.c <- "hidden"
	expect_equal( sort(runit_ls(e)), sort(c("a","b", ".c")))
	expect_equal( runit_ls2(e), c("a","b"))
})

test_that( "Environment::get is correct", {
	e <- new.env( )
	e$a <- 1:10
	e$b <- "foo"

	expect_equal( runit_get( e, "a" ), e$a)
	expect_equal( runit_get( e, "foobar" ), NULL)

})

test_that( "Environment::exists is correct", {
	e <- new.env( )
	e$a <- 1:10
	e$b <- "foo"
	expect_true( runit_exists( e, "a" ))
	expect_true( !runit_exists( e, "foobar" ))
})

test_that( "Environment::assign is correct", {
	e <- new.env( )
	runit_assign(e, "a", 1:10 )
	expect_equal( ls(e), c("a"))
	expect_equal( e$a, 1:10)
	
	lockBinding( "a", e )
	expect_error( runit_assign(e, "a", letters ) )
})

test_that( "Environment correctly handles bindings", {
	e <- new.env()
	runit_islocked(e)
	expect_equal( e[["x1"]], 1L  )
	expect_equal( e[["x2"]], 10.0)
	expect_equal( e[["x3"]], "foobar")
	expect_equal( e[["x4"]], "foobar")
	expect_equal( e[["x5"]], c("foo", "bar" ))
	
	e <- new.env()
	e$a <- 1:10
	makeActiveBinding( "b", function(x) 10, e )

	expect_true( !runit_bindingIsActive(e, "a" ))
	expect_true( runit_bindingIsActive(e, "b" ))

	expect_error( runit_bindingIsActive(e, "xx" ) )

	e <- new.env()
	e$a <- 1:10
	e$b <- letters
	lockBinding( "b", e )

	expect_true( !runit_bindingIsLocked(e, "a" ))
	expect_true( runit_bindingIsLocked(e, "b" ))
	expect_error( runit_bindingIsLocked(e, "xx" ))
	
})

test_that( "Environment can lock and unlock bindings", {
	e <- new.env()
	e$a <- 1:10
	e$b <- letters
	runit_lockbinding(e, "b")
	expect_true( bindingIsLocked("b", e ))
	expect_error( runit_lockbinding(e, "xx" ) )

	runit_unlockbinding(e, "b")
	expect_true( !bindingIsLocked("b", e ))
	expect_error( runit_unlockbinding(e, "xx" ))
})

test_that( "Environment finds known environments", {
	expect_equal( runit_globenv(), globalenv())
	expect_equal( runit_emptyenv(), emptyenv())
	expect_equal( runit_baseenv(), baseenv())
})

test_that( "Environment handles namespaces", {
  	expect_equal( runit_namespace("methods"), asNamespace("methods"))
  	expect_error( runit_namespace("----" ) )
})

test_that( "Environment handles SEXP", {
  	expect_equal( runit_env_SEXP( globalenv() ), globalenv())
	expect_equal( runit_env_SEXP( baseenv() ), baseenv())
	expect_equal( runit_env_SEXP( asNamespace("methods") ), asNamespace("methods"))

	expect_equal( runit_env_SEXP( ".GlobalEnv" ), globalenv())
	expect_equal( runit_env_SEXP( "package:base" ), baseenv())
	
	expect_equal( runit_env_SEXP(1L), globalenv())
  
	expect_error( runit_notanenv( runit_notanenv ))
	expect_error( runit_notanenv( letters ))
	expect_error( runit_notanenv( NULL ))
})

test_that( "Environment handles search position", {
	for( i in seq_along(search())){
		expect_equal( runit_env_int(i), as.environment(i))
	}
})

test_that( "Environment can remove variables from itself", {
	e <- new.env( )
	e$a <- 1
	e$b <- 2
	expect_true( runit_remove( e, "a" ))
	expect_equal( ls(envir=e), "b")
	expect_error( runit_remove(e, "xx"))
	lockBinding( "b", e )
	expect_error( runit_remove(e, "b"))
	expect_equal( ls(envir=e), "b")
})

test_that( "Environment knows its parent and child", {
	e <- new.env( parent = emptyenv() )
	f <- new.env( parent = e )
	expect_equal( runit_parent(f), e)
	expect_equal( runit_parent(e), emptyenv() )
	expect_equal( parent.env(runit_child()), globalenv())
})

test_that( "Environment implements extraction", {
	env <- new.env( )
	env[["x"]] <- 10L
	expect_equal( runit_square(env), list( 10L, 2L, "foo") )
})

