#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rcpp_get_composition_vector(const Rcpp::NumericVector &x)
{
    return table(na_omit(x));
}

/*** R
library(raster)
new_r = raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
plot(new_r)
new_m = as.matrix(new_r)
rcpp_get_composition_vector(new_m)
*/
