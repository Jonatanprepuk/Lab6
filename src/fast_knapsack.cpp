// TO compile run Rcpp::compileAttributes()
// Then: 
// devtools::document()
// devtools::load_all()

// TEMPLATE CODE.
#include <Rcpp.h>
using namespace Rcpp;

// THIS documentation will be exported with roxygen2 

//' Add two integers using C++
//'
//' @param x First integer.
//' @param y Second integer.
//' @return The sum of x and y.
//' @export
// [[Rcpp::export]]
int add_cpp(int x, int y) {
    return x + y;
}