#include <Rcpp.h>
using namespace Rcpp ;

inline double square( double x){ return x*x; }

// [[Rcpp::export]]
RawVector raw_(){
    RawVector x(10) ;
    for( int i=0; i<10; i++) x[i] = (Rbyte)i ;
    return x ;
}

// [[Rcpp::export]]
RawVector raw_REALSXP( RawVector x ){
    for( int i=0; i<x.size(); i++) {
       x[i] = x[i]*2 ;
    }
    return x ;
}

// [[Rcpp::export]]
ExpressionVector expression_(){
    ExpressionVector x(2) ;
    x[0] = Symbol( "rnorm" ) ;
    x[1] = Rf_lcons( Symbol("rnorm"), Rf_cons( Rf_ScalarReal(10.0), R_NilValue) ) ;
    return x ;
}

// [[Rcpp::export]]
ExpressionVector expression_variadic(){
    ExpressionVector x(2) ;
    x[0] = Symbol( "rnorm" ) ;
    x[1] = Language( "rnorm", 10.0 ) ;
    return x ;
}

// [[Rcpp::export]]
ExpressionVector expression_parse(){
    ExpressionVector code( "local( { y <- sample(1:10); sort(y) })" ) ;
    return code ;
}

// [[Rcpp::export]]
ExpressionVector expression_parseerror(){
    ExpressionVector code( "rnorm(" ) ;
    return code ;
}

// [[Rcpp::export]]
ComplexVector complex_(){
    ComplexVector x(10) ;
    Rcomplex rc ;
    for( int i=0; i<10; i++) {
        rc.r = rc.i = i + 0.0 ;
        x[i] = rc ;
    }       
    return x ;
}

// [[Rcpp::export]]
ComplexVector complex_CPLXSXP( ComplexVector x ){
    int nn = x.size();
    for( int i=0; i<nn; i++) {
        x[i].r = x[i].r*2 ;
        x[i].i = x[i].i*2 ;
    }
    return x ;
}

// [[Rcpp::export]]
ComplexVector complex_INTSXP( SEXP vec ){
    ComplexVector x(vec);
    int nn = x.size();
    IntegerVector tmp(nn, 2.0);
    ComplexVector tmp1(tmp);
    x = x * tmp1;
    return x ;
}

// [[Rcpp::export]]
ComplexVector complex_REALSXP(SEXP vec){
    ComplexVector x(vec);
    int nn = x.size();
    NumericVector tmp(nn, 3.0);
    ComplexVector tmp1(tmp);
    x = x * tmp1;
    return x ;
}

// [[Rcpp::export]]
IntegerVector integer_ctor(){
    IntegerVector x(10) ;
    for( int i=0; i<10; i++) x[i] = i ;
    return x ;
}

// [[Rcpp::export]]
IntegerVector integer_INTSXP(SEXP vec){
    IntegerVector x(vec) ;
    for( int i=0; i<x.size(); i++) {
        x[i] = x[i]*2 ;
    }
    return x ;
}

// [[Rcpp::export]]
IntegerVector integer_names_set(){
    IntegerVector y(2) ;
    std::vector<std::string> names(2)  ;
    names[0] = "foo" ;
    names[1] = "bar" ;
    y.names() = names ;
    return y ;    
}

// [[Rcpp::export]]
CharacterVector integer_names_get( IntegerVector y ){
    return y.names() ;
}

// [[Rcpp::export]]
int integer_names_indexing( IntegerVector y ){
    return y["foo"] ;
}

// [[Rcpp::export]]
IntegerVector integer_zero(){
    return IntegerVector(0);
}

// [[Rcpp::export]]
IntegerVector integer_create_zero(){
    return IntegerVector::create();
}

// [[Rcpp::export]]
List integer_create_(){
    List output(2);
    output[0] = IntegerVector::create( 10, 20 ) ;
    output[1] = IntegerVector::create(
        _["foo"] = 20,
        _["bar"] = 30 ) ;
    return output ;
}

// [[Rcpp::export]]
IntegerVector integer_clone_( IntegerVector vec ){
    IntegerVector dolly = clone( vec ) ;
    for( size_t i=0; i<10; i++){
        dolly[i] = 10 - i ;
    }
    return dolly ;
}

// [[Rcpp::export]]
NumericVector numeric_(){
    NumericVector x(10) ;
    for( int i=0; i<10; i++) x[i] = i ;
    return x ;
}

// [[Rcpp::export]]
NumericVector numeric_REALSXP( SEXP vec){
    NumericVector x(vec) ;
    for( int i=0; i<x.size(); i++) {
        x[i] = x[i]*2.0 ;
    }
    return x ;
}

// [[Rcpp::export]]
List list_ctor(){
    List x(10) ;
    for( int i=0; i<10; i++) x[i] = Rf_ScalarInteger( i * 2)  ;
    return x ;
}
    
// [[Rcpp::export]]
List list_template_(){
    List x(4) ;
    x[0] = "foo"  ;
    x[1] = 10 ;
    x[2] = 10.2 ;
    x[3] = false;
    return x ;
}

// [[Rcpp::export]]
List list_VECSXP_( SEXP vec){
    List x(vec) ;
    return x ;
}

// [[Rcpp::export]]
List list_iterator_( List input, Function fun){
    List output( input.size() ) ;
    std::transform( input.begin(), input.end(), output.begin(), fun ) ;
    output.names() = input.names() ;
    return output ;
}

// [[Rcpp::export]]
int list_name_indexing( List df ){
    IntegerVector df_x = df["x"] ;
    int res = std::accumulate( df_x.begin(), df_x.end(), 0 ) ;
    return res ;
}

// [[Rcpp::export]]
List list_implicit_push_back(){
    List list ;
    list["foo"] = 10 ;
    list["bar" ] = "foobar" ;
    return list ;
}

// [[Rcpp::export]]
List list_create_(){
    List output(2);
    output[0] = List::create( 10, "foo" ) ;
    output[1] = List::create(
        _["foo"] = 10,
        _["bar"] = true ) ;
    return output ;
}

// [[Rcpp::export]]
List list_stdcomplex(){
    std::vector< std::complex<double> > v_double(10) ;
    std::vector< std::complex<float> > v_float(10) ;
    return List::create( _["float"] = v_float, _["double"] = v_double ) ;
}

// [[Rcpp::export]]
CharacterVector character_ctor(){
    CharacterVector x(10) ;
    for( int i=0; i<10; i++) x[i] = "foo" ;
    return x ;
}

// [[Rcpp::export]]
std::string character_STRSXP_( SEXP vec ){
    CharacterVector x(vec) ;
    std::string st = "" ;
    for( int i=0; i<x.size(); i++) {
        st += x[i] ;
    }
    return st ;
}

// [[Rcpp::export]]
CharacterVector character_plusequals(){
    CharacterVector x(2) ;
    x[0] = "foo" ;
    x[1] = "bar" ;
    x[0] += "bar" ;
    x[1] += x[0] ;
    return x ;
}

// [[Rcpp::export]]
std::string character_iterator1( CharacterVector letters ){
    std::string res ;
    CharacterVector::iterator first = letters.begin() ;
    CharacterVector::iterator last = letters.end() ;
    while( first != last ){
        res += *first ;
        ++first ;
    }
    return res ;
}


// [[Rcpp::export]]
std::string character_iterator2( CharacterVector letters ){
    std::string res(std::accumulate(letters.begin(), letters.end(), std::string()));
    return res ;
}

// [[Rcpp::export]]
CharacterVector character_reverse( CharacterVector y ){
    std::reverse( y.begin(), y.end() ) ;
    return y ;
}

// [[Rcpp::export]]
std::string character_names_indexing( CharacterVector y ){
    std::string foo = y["foo"] ;
    return foo ;
}

// [[Rcpp::export]]
int character_find_(CharacterVector y){
    CharacterVector::iterator it = std::find( y.begin(), y.end(), "foo" ) ;
    return std::distance( y.begin(), it );
}

// [[Rcpp::export]]
List character_create_(){
    List output(2);
    output[0] = CharacterVector::create( "foo", "bar" ) ;
    output[1] = CharacterVector::create(
        _["foo"] = "bar",
        _["bar"] = "foo"
        ) ;
    return output ;
}

// [[Rcpp::export]]
List List_extract( List input ){
    bool a = input[0] ;
    int b = input[1] ;
    return List::create(a, b) ;
}

// [[Rcpp::export]]
CharacterVector factors( CharacterVector s){
    return s;
}

// [[Rcpp::export]]
IntegerVector IntegerVector_int_init(){
    IntegerVector x(2,4) ;
    return x ;
}

// [[Rcpp::export]]
List CharacterVectorEqualityOperator( CharacterVector x, CharacterVector y){
    int n = x.size() ;
    LogicalVector eq(n), neq(n);
    for( int i=0; i<n; i++){
        eq[i]  = x[i] == y[i] ;
        neq[i] = x[i] != y[i] ; 
    }
    return List::create(eq, neq) ;
}

// [[Rcpp::export]]
List List_rep_ctor(IntegerVector x){
    return List(3, x) ;
}

// [[Rcpp::export]]
int stdVectorDouble(std::vector<double> x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorDoubleConst(const std::vector<double> x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorDoubleRef(std::vector<double> & x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorDoubleConstRef(const std::vector<double> & x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorInt(std::vector<int> x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorIntConst(const std::vector<int> x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorIntRef(std::vector<int> & x) { 
    return x.size();
}

// [[Rcpp::export]]
int stdVectorIntConstRef(const std::vector<int> & x) { 
    return x.size();
}

// [[Rcpp::export]]
std::string character_vector_const_proxy(const CharacterVector& str){
    std::string res = str[0] ;
    return res ;
}

// [[Rcpp::export]]
CharacterVector CharacterVector_test_const_proxy(const CharacterVector x){
    CharacterVector out( x.size() ) ;
    for( int i=0; i<x.size(); i++){
        out[i] = x[i] ;    
    }
    return out ;
}
