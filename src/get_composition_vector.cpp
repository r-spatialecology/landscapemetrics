#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_get_composition_vector(const Rcpp::NumericVector & x)
{
    Rcpp::IntegerVector x_table = table(na_omit(x));
    return x_table;
}

/*** R
library(raster)
new_r = raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
plot(new_r)
new_m = as.matrix(new_r)
rcpp_get_composition_vector(new_m)
*/
