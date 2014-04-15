#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool Rcpp_IsNA(double x) {
    return internal::is_NA(x);
}

// [[Rcpp::export]]
bool Rcpp_IsNaN(double x) {
    return internal::is_NaN(x);
}
