#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::ivec get_neighbors(arma::imat x, int i, int j, int directions = 8) {
        // number of rows and cols
        int max_r = x.n_rows - 1;
        int max_c = x.n_cols - 1;
        // starting vector location (id)
        int loc = 0;
        // initiate the longest possible vector (8-neighbors)
        arma::ivec v(8);
        // initiate neighbors ids
        arma::ivec x_id;
        arma::ivec y_id;
        // queen (8-n) or not (4-n)
        if (directions == 8){
                x_id = {0, -1, 0, 1, -1, 1, -1, 0, 1};
                y_id = {0, -1, -1, -1, 0, 0, 1, 1, 1};
        } else {
                x_id = {0, 0, -1, 1, 0};
                y_id = {0, -1, 0, 0, 1};
        }
        // for each neighbors id (except the center)
        for (arma::uword id = 1; id < x_id.n_elem; id++) {
                // neighbor location
                int a = i + x_id(id);
                int b = j + y_id(id);
                // check if a neighbor exist
                if (a >= 0 && b >= 0 && a <= max_r && b <= max_c){
                        // extract a neighbor value
                        int val = x(a, b);
                        // check if a neighbor value is not NA (INT_MIN)
                        if (val != INT_MIN){
                                // add a neighbor value and update the next location
                                v(loc) = val;
                                loc ++;
                        }
                }
        }
        // return only existing values (if loc is shorter than 8 elements)
        return v.head(loc);
}

/*** R
library(raster)
new_r = raster(nrows = 3, ncols = 3, vals = c(2, rep(1, 7), NA))
plot(new_r)
new_m = as.matrix(new_r)
# neighbors of a central point
get_neighbors(new_m, 1, 1)
*/
