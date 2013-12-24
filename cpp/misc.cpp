#include <Rcpp.h>
using namespace Rcpp ;
using namespace std;
#include <iostream>
#include <fstream>

// [[Rcpp::export]]
SEXP symbol_(){
    return LogicalVector::create( 
        Symbol( Rf_install("foobar") ) == Rf_install("foobar"), 
        Symbol( Rf_mkChar("foobar") ) == Rf_install("foobar"), 
        Symbol( Rf_mkString("foobar") ) == Rf_install("foobar"), 
        Symbol( "foobar" ) == Rf_install("foobar") 
    ) ;
}

// [[Rcpp::export]]
Symbol symbol_ctor(SEXP x){ return Symbol(x); }

// [[Rcpp::export]]
List Argument_(){
    Argument x("x"), y("y");
    return List::create( x = 2, y = 3 );
}

// [[Rcpp::export]] 
SEXP evaluator_error(){
    return Rcpp_eval( Rf_lang2( Rf_install("stop"), Rf_mkString( "boom" ) ) ) ;
}

// [[Rcpp::export]]
SEXP evaluator_ok(SEXP x){
    return Rcpp_eval( Rf_lang2( Rf_install("sample"), x ) ) ;
}

// [[Rcpp::export]]
void exceptions_(){
    throw std::range_error("boom") ;
}

// [[Rcpp::export]]
LogicalVector has_iterator_( ){
    return LogicalVector::create( 
        (bool)Rcpp::traits::has_iterator< std::vector<int> >::value, 
        (bool)Rcpp::traits::has_iterator< std::list<int> >::value, 
        (bool)Rcpp::traits::has_iterator< std::deque<int> >::value, 
        (bool)Rcpp::traits::has_iterator< std::set<int> >::value, 
        (bool)Rcpp::traits::has_iterator< std::map<std::string,int> >::value, 
        (bool)Rcpp::traits::has_iterator< std::pair<std::string,int> >::value, 
        (bool)Rcpp::traits::has_iterator< Rcpp::Symbol >::value 
        );
}

// [[Rcpp::export]]
LogicalVector na_proxy(){
    CharacterVector s("foo") ;
    return LogicalVector::create( 
        NA_REAL    == NA, 
        NA_INTEGER == NA,
        NA_STRING  == NA,
        true       == NA, 
        false      == NA, 
        1.2        == NA, 
        12         == NA,
        "foo"      == NA,
        s[0]       == NA, 
        
        NA         == NA_REAL, 
        NA         == NA_INTEGER,
        NA         == NA_STRING,
        NA         == true, 
        NA         == false,
        NA         == 1.2  , 
        NA         == 12   ,
        NA         == "foo", 
        NA         == s[0]
        ) ;
}      

// [[Rcpp::export]]
StretchyList stretchy_list(){
    StretchyList out ;
    out.push_back( 1 ) ;
    out.push_front( "foo" ) ;
    out.push_back( 3.2 ) ;
    return out;
}

// [[Rcpp::export]]
StretchyList named_stretchy_list(){
    StretchyList out ;
    out.push_back( _["b"] = 1 ) ;
    out.push_front( _["a"] = "foo" ) ;
    out.push_back( _["c"] = 3.2 ) ;
    return out;
}

// [[Rcpp::export]]
std::string runit_Reference_getId(Reference obj) {
    std::string txt = obj.field("id");
    return txt;
}

// [[Rcpp::export]]
List plus_REALSXP(){
    return List::create(
        NA_REAL + NA_REAL,
        NA_REAL + 1.0,
        1.0 + NA_REAL
    );
}

// [[Rcpp::export]]
List times_REALSXP(){
    return List::create(
        NA_REAL * NA_REAL,
        NA_REAL * 1.0,
        1.0 * NA_REAL
    );
}

// [[Rcpp::export]]
List divides_REALSXP(){
    return List::create(
       NA_REAL / NA_REAL,
       NA_REAL / 1.0,
       1.0 / NA_REAL
       );
}

// [[Rcpp::export]]
List minus_REALSXP(){
    return List::create(
       NA_REAL - NA_REAL,
       NA_REAL - 1.0,
       1.0 - NA_REAL
       );
}

// [[Rcpp::export]]
List functions_REALSXP(){
    return List::create(
        NumericVector::create(
           exp( NA_REAL ),
           acos( NA_REAL ),
           asin( NA_REAL ),
           atan( NA_REAL ),
           ceil( NA_REAL ),
           cos( NA_REAL ),
           cosh( NA_REAL ),
           floor( NA_REAL ),
           log( NA_REAL ),
           log10( NA_REAL ),
           sqrt( NA_REAL),
           sin( NA_REAL ),
           sinh( NA_REAL ),
           tan( NA_REAL ),
           tanh( NA_REAL ),
           fabs( NA_REAL ),
           Rf_gammafn( NA_REAL),
           Rf_lgammafn( NA_REAL ),
           Rf_digamma( NA_REAL ),
           Rf_trigamma( NA_REAL )
        ) , NumericVector::create(
           Rf_tetragamma( NA_REAL) ,
           Rf_pentagamma( NA_REAL) ,
           expm1( NA_REAL ),
           log1p( NA_REAL ),
           Rcpp::internal::factorial( NA_REAL ),
           Rcpp::internal::lfactorial( NA_REAL )
        )
     );
}

// [[Rcpp::export]]
List S4_methods( RObject y ){
    List res(5) ;
    res[0] = y.isS4() ;
    res[1] = y.hasSlot("x") ;
    res[2] = y.hasSlot("z") ;
    res[3] = y.slot("x") ;
    res[4] = y.slot("y") ;
    return res ;        
}

// [[Rcpp::export]]
void S4_getslots( S4 y){
    y.slot( "x" ) = 10.0 ;
    y.slot( "y" ) = 20.0 ;
}      

// [[Rcpp::export]]
void S4_setslots( S4 y ){
    y.slot( "foo" ) = 10.0 ;
}

// [[Rcpp::export]]
void S4_setslots_2( S4 y){
    y.slot( "foo" ) ;    
}

// [[Rcpp::export]]
S4 S4_ctor( std::string cl){
    return S4( cl );    
}

// [[Rcpp::export]]
bool S4_is_track(S4 tr){
    return tr.is("track") ;
}

// [[Rcpp::export]]
bool S4_is_trackCurve(S4 tr){
    return tr.is("trackCurve") ;
}

// [[Rcpp::export]]
NumericVector S4_get_slot_x(S4 o){
    NumericVector res( o.slot("x") );
    return res ;    
}

// [[Rcpp::export]]
CharacterVector S4_get_attr_x(IntegerVector o){
    CharacterVector res( o.attr("foo") ) ;
    return res ;
}

// [[Rcpp::export]]
S4 S4_dotdata(S4 foo){
    foo.slot( ".Data" ) = "foooo" ;
    return foo ;
}

// [[Rcpp::export]]
int countArgs(Dots dots){
    return dots.size() ;    
}

// [[Rcpp::export]]
List countNamedArgs( NamedDots dots){
    CharacterVector names( dots.size() ) ;
    int n = dots.size() ;
    for( int i=0; i<n; i++){
        names[i] = String( dots.symbol(i) ) ;
    }
    return List::create( 
        _["count"] = n, 
        _["names"] = names
        ) ;
}

