#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
Function function_(SEXP x){ return Function(x) ; }

// [[Rcpp::export]]
NumericVector function_variadic(Function sort, NumericVector y){
    return sort( y, Named("decreasing", true) ) ;
}

// [[Rcpp::export]]
Environment function_env(Function fun){
    return fun.environment() ;
}

// [[Rcpp::export]]
Function function_namespace_env(){
    Environment ns = Environment::namespace_env( "stats" ) ;
    Function fun = ns[".asSparse"] ;  // accesses a non-exported function
    return fun;
}

