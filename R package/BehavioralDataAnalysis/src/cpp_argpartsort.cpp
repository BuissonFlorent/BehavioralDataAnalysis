
#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericVector cpp_argpartsort(NumericVector x, int n) {
    NumericVector y = clone(x);
    std::partial_sort(y.begin(), y.begin()+n, y.end());
    return y;
}
