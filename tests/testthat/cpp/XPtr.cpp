#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
XPtr< std::vector<int> > xptr_1(){
		    /* creating a pointer to a vector<int> */
		    std::vector<int>* v = new std::vector<int> ;
		    v->push_back( 1 ) ;
		    v->push_back( 2 ) ;
        
		    /* wrap the pointer as an external pointer */
		    /* this automatically protected the external pointer from R garbage
		       collection until p goes out of scope. */
		    XPtr< std::vector<int> > p(v) ;
        
		    /* return it back to R, since p goes out of scope after the return
		       the external pointer is no more protected by p, but it gets
		       protected by being on the R side */
		    return( p ) ;
}

// [[Rcpp::export]]
int xptr_2( XPtr< std::vector<int> > p){
    		/* just return the front of the vector as a SEXP */
    		return p->front() ;
}

