#include <Rcpp.h>
using namespace Rcpp ;


// [[Rcpp::export]]
String String_replace_all( String z, String x, String y){
    z.replace_all( x, y ) ;
    return z ;
}

// [[Rcpp::export]]
String String_replace_first( String z, String x, String y){
    z.replace_first( x, y ) ;
    return z ;
}
// [[Rcpp::export]]
String String_replace_last( String z, String x, String y){
    z.replace_last( x, y ) ;
    return z ;
}

class StringConv{
public:
    typedef String result_type ;
    StringConv( CharacterVector old_, CharacterVector new__): 
        nr(old_.size()), old(old_), new_(new__){}
    
    String operator()(String text) const {
        for( int i=0; i<nr; i++){
            text.replace_all( old[i], new_[i] ) ;
        }     
        return text ;
    }
    
private:
    int nr ;
    CharacterVector old ;
    CharacterVector new_ ;
} ;

// [[Rcpp::export]]
CharacterVector test_sapply_string( CharacterVector text, CharacterVector old , CharacterVector new_){
   CharacterVector res = sapply( text, StringConv( old, new_ ) ) ;
   return res ;
}  

// [[Rcpp::export]]
List test_compare_Strings( String aa, String bb ){
    return List::create(
        _["a  < b" ] = aa < bb, 
        _["a  > b" ] = aa > bb, 
        _["a == b"]  = aa == bb,
        _["a == a"]  = aa == aa
        ) ;
}

// [[Rcpp::export]]
CharacterVector CharacterVector_wstring( ){
    CharacterVector res(2) ;
    res[0] = L"foo" ;
    res[0] += L"bar" ;
              
    res[1] = std::wstring( L"foo" ) ;
    res[1] += std::wstring( L"bar" ) ;
         
    return res ;
}
            
// [[Rcpp::export]]
std::wstring wstring_return(){
    return L"foo" ;    
}

// [[Rcpp::export]]
String wstring_param(std::wstring s1, std::wstring s2){
    String s = s1 ;
    s += s2 ;
    return s ;
}

// [[Rcpp::export]]
std::vector<std::wstring> wrap_vector_wstring(){
    std::vector<std::wstring> res(2 ); 
    res[0] = L"foo" ;
    res[1] = L"bar" ;
    return res;
}

// [[Rcpp::export]]
std::vector<std::wstring> as_vector_wstring( std::vector<std::wstring> x){
    for( size_t i=0; i<x.size(); i++){
        x[i] += L"€" ;    
    }
    return x ;        
}

