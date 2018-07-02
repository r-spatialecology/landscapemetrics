#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericVector rcpp_get_composition_vector(arma::imat x) {
        // number of rows and cols
        int num_r = x.n_rows;
        int num_c = x.n_cols;
        // get unique values
        arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));
        // create a vector of zeros of unique values size
        arma::ivec comp_vec(u.n_elem, arma::fill::zeros);
        // for each row and col
        for (int i = 0; i < num_r; i++) {
                for (int j = 0; j < num_c; j++) {
                        // extract a central value
                        int center = x(i, j);
                        // add a new count in the central value place
                        comp_vec(find(center == u)) += 1;
                }
        }
        // return a composition vector
        NumericVector result = as<NumericVector>(wrap(comp_vec));
        // remove a dim attribute
        result.attr("names") = u;
        result.attr("dim") = R_NilValue;
        return result;
}

/*** R
library(raster)
new_r = raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
plot(new_r)
new_m = as.matrix(new_r)
rcpp_get_composition_vector(new_m)

new_r2 = raster(nrows = 3, ncols = 2, vals = c(2, rep(1, 4), NA))
plot(new_r2)
new_m2 = as.matrix(new_r2)
rcpp_get_composition_vector(new_m2)

new_r3 = raster(nrows = 3, ncols = 3, vals = c(3, rep(1, 7), NA))
plot(new_r3)
new_m3 = as.matrix(new_r3)
rcpp_get_composition_vector(new_m3)

new_r4 = raster(nrows = 3, ncols = 3, vals = c(2, rep(1, 6), 3, 1))
new_m4 = as.matrix(new_r4)
rcpp_get_composition_vector(new_m4)
*/
