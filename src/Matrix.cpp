#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
double matrix_numeric( NumericMatrix m){
    double trace = 0.0 ;
    for( size_t i=0 ; i<4; i++){
        	trace += m(i,i) ;
    }
    return trace ;
}

// [[Rcpp::export]]
std::string matrix_character( CharacterMatrix m){
    std::string trace ;
    for( size_t i=0 ; i<4; i++){
        	trace += m(i,i) ;
    }
    return trace;
}

// [[Rcpp::export]]
List matrix_generic( GenericMatrix m){
    List output( m.ncol() ) ;
    for( size_t i=0 ; i<4; i++){
        	output[i] = m(i,i) ;
    }
    return output ;
}

// [[Rcpp::export]]
NumericMatrix matrix_numeric_ctor2(){
    return NumericMatrix(3,3,0.0);
}

// [[Rcpp::export]]
int integer_matrix_indexing( IntegerMatrix m){
    int trace = 0.0 ;
    for( size_t i=0 ; i<4; i++){
        trace += m(i,i) ;
    }
    return trace ;
}

// [[Rcpp::export]]
IntegerMatrix integer_matrix_indexing_lhs( IntegerMatrix m ){
    for( size_t i=0 ; i<4; i++){
        m(i,i) = 2 * i ;
    }
    return m ;
}

// [[Rcpp::export]]
double runit_NumericMatrix_row( NumericMatrix m){
    NumericMatrix::Row first_row = m.row(0) ;
    return std::accumulate( first_row.begin(), first_row.end(), 0.0 ) ;
}

// [[Rcpp::export]]
std::string runit_CharacterMatrix_row( CharacterMatrix m ){
    CharacterMatrix::Row first_row = m.row(0) ;
    std::string res(
    	std::accumulate(
    		first_row.begin(), first_row.end(), std::string() ) ) ;
    return res ;
}
  
// [[Rcpp::export]]
double runit_NumericMatrix_column( NumericMatrix m ){
    NumericMatrix::Column col = m.column(0) ;
    return std::accumulate( col.begin(), col.end(), 0.0 ) ;
}

// [[Rcpp::export]]   
NumericMatrix runit_NumericMatrix_cumsum( NumericMatrix input ){
    int nr = input.nrow(), nc = input.ncol() ;
    NumericMatrix output(nr, nc) ;
    NumericVector tmp( nr, 0 );
    for( int i=0; i<nc; i++){
        tmp = tmp + input.column(i) ;
        // NumericMatrix::Column target( output, i ) ;
        // std::copy( tmp.begin(), tmp.end(), target.begin() ) ;
        output(_,i) = tmp ;
    }
    return output ;
}

// [[Rcpp::export]]
std::string runit_CharacterMatrix_column( CharacterMatrix m){
    CharacterMatrix::Column col = m.column(0) ;
    std::string res(
        std::accumulate( col.begin(), col.end(), std::string() )
    ) ;
    return res ;
}

// [[Rcpp::export]]
List runit_Row_Column_sugar( NumericMatrix x){
    NumericVector r0 = x.row(0) ;
    NumericVector c0 = x.column(0) ;
    return List::create(
        r0,
        c0,
        x.row(1),
        x.column(1),
        x.row(1) + x.column(1)
        ) ;
}

// [[Rcpp::export]]
NumericMatrix runit_NumericMatrix_colsum( NumericMatrix input ){
    int nc = input.ncol() ;
    NumericMatrix output = clone<NumericMatrix>( input ) ;
    for( int i=1; i<nc; i++){
       output(_,i) = output(_,i-1) + input(_,i) ;
    }
    return output ;
}

// [[Rcpp::export]]
NumericMatrix runit_NumericMatrix_rowsum( NumericMatrix input ){
    int nr = input.nrow();
    NumericMatrix output = clone<NumericMatrix>( input ) ;
    for( int i=1; i<nr; i++){
       output(i,_) = output(i-1,_) + input(i,_) ;
    }
    return output ;
}

// [[Rcpp::export]]
CharacterVector character_matrix_indexing( CharacterMatrix m ){
    std::string trace;
    for( size_t i=0 ; i<4; i++){
        trace += m(i,i) ;
    }
    return wrap( trace ) ;
}

// [[Rcpp::export]]
CharacterMatrix character_matrix_indexing_lhs( CharacterMatrix m ){
    for( size_t i=0 ; i<4; i++){
        m(i,i) = "foo" ;
    }
    return m ;
}

// [[Rcpp::export]]
CharacterVector character_matrix_row_iteration_incr( CharacterMatrix m ){
    std::string pasted_row;
    CharacterMatrix::Row row(m(1, _));
    CharacterMatrix::Row::iterator i_row(row.begin());
    for( size_t i=0 ; i<4; i++){
        pasted_row += *i_row++;
    }
    return wrap( pasted_row ) ;
}

// [[Rcpp::export]]
CharacterVector character_matrix_row_iteration_decr( CharacterMatrix m ){
    std::string pasted_row;
    CharacterMatrix::Row row(m(1, _));
    CharacterMatrix::Row::iterator i_row(row.end());
    i_row--; // Step back from 'one past the end' to 'last element'.
    // Only copy the last three elements, to avoid creating an invalid
    // 'one before the beginning' iterator:
    for( size_t i=0 ; i<3; i++){
        pasted_row += *i_row--;
    }
    return wrap( pasted_row ) ;
}


