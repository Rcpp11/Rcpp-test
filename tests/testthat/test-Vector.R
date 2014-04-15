context( "Vector" )


test_that( "Vector(int) constructor work", { 
    expect_equal( raw_(), as.raw(0:9))
    expect_equal( raw_REALSXP(as.raw(0:9)), as.raw(2*0:9))
    ex <- parse( text = "rnorm; rnorm(10)" )
    # get rid of the srcref stuff so that we can compare more easily
    attributes(ex) <- NULL
    expect_equal( expression_(),  ex )
    expect_equal( complex_(), 0:9*(1+1i))
    expect_equal( integer_ctor(), 0:9)
    expect_equal( integer_zero(), integer(0))
    expect_equal( numeric_(), as.numeric(0:9))
    expect_equal( list_ctor(), as.list( 2*0:9)) 
    expect_equal( character_ctor(), rep("foo",10L))
})
   
test_that( "Vector( int, init) works", {
    expect_equal( IntegerVector_int_init(), c(4L,4L))
      
    x <- 1:10
    res <- List_rep_ctor(x)
    expected <- rep( list(x), 3 )
    expect_equal( res, expected)
    
})

test_that( "Vector(SEXP) works", {
    vv = (0:9)*(1+1i)
    expect_equal( complex_CPLXSXP((0:9)*(1+1i)), 2*(0:9)*(1+1i))
    vv <- 0L:9L
    expect_equal( complex_INTSXP(vv), (2+0i)*vv)
    vv <- as.numeric(0:9)
    expect_equal( complex_REALSXP(vv), (3+0i)*vv)
    expect_equal( integer_INTSXP(0:9), 2*0:9)
    expect_equal( numeric_REALSXP(as.numeric(0:9)), 2*0:9)
    expect_equal( list_VECSXP_(list(1,2)), list(1,2))
    expect_equal( character_STRSXP_(letters), paste(letters,collapse="" ))
})

test_that( "Expression vector proxies can be assigned symbols and calls", {
    ex <- parse( text = "rnorm; rnorm(10)" )
    attributes(ex) <- NULL
    expect_equal( expression_variadic(),  ex )
})

test_that( "ExpressionVector can parse", { 
    code <- expression_parse()
    results <- eval( code )
    expect_equal( results, 1:10)
    expect_error( expression_parseerror())
})

test_that( "Vector supports names", { 
    expect_equal(names(integer_names_set()), c("foo", "bar"))
    expect_equal(
      integer_names_get( c("foo" = 1L, "bar" = 2L) ),
      c("foo", "bar")
    )
    x <- c( "foo" = 1L, "bar" = 2L )
    expect_equal( integer_names_indexing( x ), 1L)
    
    d <- data.frame( x = 1:10, y = letters[1:10] )
    expect_equal( list_name_indexing( d ), sum(1:10))
    expect_equal( list_implicit_push_back(), list( foo = 10, bar = "foobar" ))  
    x <- c( foo = "foo", bar = "bar" )
    expect_equal( character_names_indexing(x), "foo")
    
})

test_that( "Vector::create works", {
  expect_equal( integer_create_zero(), integer(0))
  expect_equal( integer_create_(), list( c( 10L, 20L) , c(foo = 20L, bar = 30L) ) )
  expect_equal( 
    list_create_(), 
    list( list( 10L, "foo" ), list(foo = 10L, bar =  TRUE ) )
  )
  expect_equal( 
    character_create_(), 
    list( c( "foo", "bar" ), c(foo = "bar", bar = "foo" ) )
  )
})

test_that( "clone works", {
    x <- 1:10
    y <- integer_clone_(x)
    expect_equal( x, 1:10)
    expect_equal( y, 10:1)
})     

test_that( "List supports stl-like iterators", {
    data <- list( x = letters, y = LETTERS, z = 1:4 )
    expect_equal(list_iterator_( data, length ),
                list( x = 26L, y = 26L, z = 4L),
                msg = "c++ version of lapply" )
})

test_that( "wrap handles std::complex", {
    expect_equal(
        list_stdcomplex(),
        list( float = rep(0+0i, 10), double = rep(0+0i, 10) )
    )
})

test_that( "CharacterVector proxies handle comoiund operators", {
    expect_equal( character_plusequals(), c("foobar", "barfoobar"))
})

test_that( "CharacterVector proxy work", {
    expect_equal(character_iterator1(letters),
                paste(letters, collapse="") )

    expect_equal(character_iterator2(letters),
                paste(letters, collapse="") )
    x <- c("foo", "bar", "bling")
    x <- character_reverse(x)
    expect_equal( x, c("bling", "bar", "foo"))
    x <- character_reverse(x)
    expect_equal( x, c("foo", "bar", "bling"))
    expect_equal( character_find_( c("bar", "foo", "bob") ), 1L)
})

test_that( "List proxy can expand to bool and int", {
    expect_equal( List_extract(list(TRUE, 4)), list(TRUE, 4L) )
    expect_equal( List_extract(list(FALSE, -4L)), list(FALSE,-4L) )
})

test_that( "factors can be coerced to CharacterVector", {
    x <- as.factor( c("c3", "c2", "c1") )
    y <- factors(x)
    expect_equal( y, as.character(x) )
})

test_that( "CharacterVector proxy comparison", {
    expect_equal( 
      CharacterVectorEqualityOperator( letters, letters ),
      list( rep( TRUE, 26L ), rep( FALSE, 26L) )
    )
})

test_that( "std::vector can be used as parameters in attributes", {
    x <- seq(1.0, 5.0, by=1.0)
    expect_equal(stdVectorDouble(x), 5)
    expect_equal(stdVectorDoubleConst(x), 5)
    expect_equal(stdVectorDoubleRef(x), 5)
    expect_equal(stdVectorDoubleConstRef(x), 5)

    x <- seq(1L, 5L, by=1L)
    expect_equal(stdVectorInt(x), 5)
    expect_equal(stdVectorIntConst(x), 5)
    expect_equal(stdVectorIntRef(x), 5)
    expect_equal(stdVectorIntConstRef(x), 5)
})

test_that( "CharacterVector const proxy work", {
    expect_equal( character_vector_const_proxy( "fooo" ), "fooo")
    expect_equal( CharacterVector_test_const_proxy(letters), letters )
})

