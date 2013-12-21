#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
int as_int( SEXP x){ return as<int>( x ); }

// [[Rcpp::export]]
double as_double( SEXP x){ return as<double>( x ); }

// [[Rcpp::export]]
Rbyte as_raw( SEXP x){ return as<Rbyte>( x ); }

// [[Rcpp::export]]
bool as_bool( SEXP x){ return as<bool>( x ); }

// [[Rcpp::export]]
std::string as_string( SEXP x){ return as<std::string>( x ); }

// [[Rcpp::export]]
std::vector<int> as_vector_int( SEXP x){ return as< std::vector<int> >(x) ; }

// [[Rcpp::export]]
std::vector<double> as_vector_double( SEXP x){ return as< std::vector<double> >(x) ; }

// [[Rcpp::export]]
std::vector<Rbyte> as_vector_raw( SEXP x){ return as< std::vector<Rbyte> >(x) ; }

// [[Rcpp::export]]
std::vector<bool> as_vector_bool( SEXP x){ return as< std::vector<bool> >(x) ; }

// [[Rcpp::export]]
std::vector<std::string> as_vector_string( SEXP x){ return as< std::vector<std::string> >(x) ; }

// [[Rcpp::export]]
std::deque<int> as_deque_int( SEXP x){ return as< std::deque<int> >(x) ; }

// [[Rcpp::export]]
std::list<int> as_list_int( SEXP x){ return as< std::list<int> >(x) ; }

