#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
DataFrame FromSEXP( SEXP x){
    DataFrame df(x) ;
    return df;
}

// [[Rcpp::export]]
SEXP index_byName( DataFrame df, std::string s ){
    return df[s];
}

// [[Rcpp::export]]
SEXP index_byPosition( DataFrame df, int i ){
    return df[i];
}
// [[Rcpp::export]]
std::string string_element( DataFrame df ){
    CharacterVector b = df[1];
	std::string s;
	s = b[1];
	return s;
}

// [[Rcpp::export]]
DataFrame createOne(){
    IntegerVector v = IntegerVector::create(1,2,3);
	return DataFrame::create(Named("a")=v);
}

// [[Rcpp::export]]
DataFrame createTwo(){
    IntegerVector v = IntegerVector::create(1,2,3);
	std::vector<std::string> s(3);
	s[0] = "a";
	s[1] = "b";
	s[2] = "c";
	return DataFrame::create(Named("a")=v, Named("b")=s);
}

// [[Rcpp::export]]
DataFrame SlotProxy( S4 o, std::string yy ){
    return DataFrame( o.slot( yy ) ) ;
}

// [[Rcpp::export]]
DataFrame AttributeProxy( List o, std::string y ){
    return DataFrame( o.attr( y )) ;
}

// [[Rcpp::export]]
DataFrame createTwoStringsAsFactors(){
    IntegerVector v = IntegerVector::create(1,2,3);
	std::vector<std::string> s(3);
	s[0] = "a";
	s[1] = "b";
	s[2] = "c";
	return DataFrame::create(
		_["a"] = v,
		_["b"] = s,
		_["stringsAsFactors"] = false );
}

// [[Rcpp::export]]
int DataFrame_nrows( DataFrame df){
    return df.nrows() ;
}

// [[Rcpp::export]]
DataFrame DataFrame_proxies( DataFrame df){
    df[0] = seq(1, df.nrows() ) ;
    df[1] = 3 ;

    df["z"]  = rep(2, 5) ;
    df["x3"] = seq(1, df.nrows() ) ;
    df["x4"] = 2.0 ;
    return df ;
}


