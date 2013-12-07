#include <Rcpp.h>

int bar(int x) {                // simple free function returning an int
    return x*2;
}
        
double foo(int x, double y) {   // simple free function returning a double
    return x * y;
}

void bla( ) {                   // no input or return but output side effect
    Rprintf( "hello\n" );
}

void bla1(int x) {              // output reflecting a single input
    Rprintf( "hello (x = %d)\n", x);
}
  
void bla2(int x, double y) {    // output reflecting two inputs
    Rprintf( "hello (x = %d, y = %5.2f)\n", x, y);
}


class World {                   // a simple class with a setter and getter
public:
    World() : msg("hello"){}
    void set(std::string msg) { this->msg = msg; }
    std::string greet() { return msg; }

private:
    std::string msg;
};



RCPP_MODULE(yada){
    using namespace Rcpp;
	                  
    function( "bar"   , &bar   );
    function( "foo"   , &foo   );
    function( "bla"   , &bla   );
    function( "bla1"  , &bla1   );
    function( "bla2"  , &bla2   );
	
    class_<World>( "World" )
        .default_constructor()
        .method( "greet", &World::greet )
        .method( "set", &World::set )
	;
}                     


