#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
double test_Strict( Strict<NumericVector> x ){
    return sum(x.get()) ;
}
