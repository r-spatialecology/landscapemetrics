#include <RcppArmadillo.h>
using namespace Rcpp;

#include "get_neighbors.h"

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix rcpp_get_coocurrence_matrix(arma::imat x, int directions = 8) {
        // number of rows and cols
        int num_r = x.n_rows;
        int num_c = x.n_cols;
        // get unique values
        arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));
        // create a matrix of zeros of unique values size
        arma::imat cooc_mat(u.n_elem, u.n_elem, arma::fill::zeros);
        // for each row and col
        for (int i = 0; i < num_r; i++) {
                for (int j = 0; j < num_c; j++) {
                        // extract a central value
                        int center = x(i, j);
                        // check if a central value is not NA (INT_MIN)
                        if (center != INT_MIN){
                                // get neighbors values
                                arma::ivec neighs = get_neighbors(x, i, j, directions);
                                // populate a coocurence matrix with values
                                for (arma::uword n = 0; n < neighs.n_elem; n++){
                                        arma::uvec loc_c = find(u == center);
                                        arma::uvec loc_n = find(u == neighs(n));
                                        cooc_mat(loc_c, loc_n) += 1;
                                }
                        }
                }
        }
        // return a coocurence matrix
        NumericMatrix cooc_mat_result = as<NumericMatrix>(wrap(cooc_mat));
        // add names
        List u_names = List::create(u, u);
        cooc_mat_result.attr("dimnames") = u_names;
        return cooc_mat_result;
}

// [[Rcpp::export]]
int triangular_index(int r, int c) {
        r++;
        c++;
        if (c <= r){
          return (r - 1) * r / 2 + c - 1;
        } else {
          return (c - 1) * c / 2 + r - 1;
        }
}

// [[Rcpp::export]]
NumericVector rcpp_get_coocurrence_vector(arma::imat x, int directions = 8, bool ordered = true) {
        NumericVector result;
        // calculate a coocurrence matrix
        x = as<arma::imat>(rcpp_get_coocurrence_matrix(x, directions));
        if (ordered){
                result = as<NumericVector>(wrap(x));
        } else {
                // get a coocurence matrix dimension (it is equal to nrow and ncol)
                int num_e = x.n_cols - 1;
                // Unique combinations number
                int uc = triangular_index(num_e, num_e) + 1;
                // create an empty vector of the unique combinations size
                NumericVector hist(uc);
                // populate a histogram
                for (int i = 0; i <= num_e; i++) {
                        for (int j = 0; j <= num_e; j++) {
                                hist(triangular_index(i, j)) += x(i, j);
                        }
                }
                // every value of neighborhood was calculated twice, therefore divide by 2
                // return a coocurrence vector
                result = as<NumericVector>(wrap(hist / 2));
        }
        // remove a dim attribute
        result.attr("dim") = R_NilValue;
        return result;
}

// [[Rcpp::export]]
NumericVector rcpp_get_offdiagonal_vector(arma::imat x, int directions = 8) {
        // calculate a coocurrence matrix
        x = as<arma::imat>(rcpp_get_coocurrence_matrix(x, directions));
        // extract off-diagonal
        arma::ivec offdiag = arma::conv_to<arma::ivec>::from(x.elem(find(trimatl(x, -1) != 0)));
        // return a vector
        NumericVector result = as<NumericVector>(wrap(offdiag));
        // remove a dim attribute
        result.attr("dim") = R_NilValue;
        return result;
}

/*** R
library(raster)
# prepare data
new_r = raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
plot(new_r)
new_m = as.matrix(new_r)
new_r2 = raster(nrows = 3, ncols = 2, vals = c(2, rep(1, 4), NA))
new_m2 = as.matrix(new_r2)
new_r3 = raster(nrows = 3, ncols = 3, vals = c(3, rep(1, 7), NA))
new_m3 = as.matrix(new_r3)
new_r4 = raster(nrows = 3, ncols = 3, vals = c(2, rep(1, 6), 3, 1))
new_m4 = as.matrix(new_r4)

# coocurrence matrix
rcpp_get_coocurrence_matrix(new_m)
rcpp_get_coocurrence_matrix(new_m, 4)

rcpp_get_coocurrence_matrix(new_m2)
rcpp_get_coocurrence_matrix(new_m2, 4)

rcpp_get_coocurrence_matrix(new_m3)
rcpp_get_coocurrence_matrix(new_m3, 4)

rcpp_get_coocurrence_matrix(new_m4)
rcpp_get_coocurrence_matrix(new_m4, 4)

# coocurrence vector
rcpp_get_coocurrence_vector(new_m) #8-n
rcpp_get_coocurrence_vector(new_m, 4) #4-n

rcpp_get_coocurrence_vector(new_m, ordered = FALSE)
rcpp_get_coocurrence_vector(new_m, ordered = TRUE)

# offdiagonal vector
rcpp_get_offdiagonal_vector(new_m) #8-n
rcpp_get_offdiagonal_vector(new_m, 4) #4-n

rcpp_get_offdiagonal_vector(new_m4) #8-n
rcpp_get_offdiagonal_vector(new_m4, 4) #8-n

# triangular_index tests
triangular_index(0, 0)
triangular_index(1, 0)
triangular_index(2, 0)
triangular_index(3, 0)
triangular_index(0, 1)
triangular_index(0, 2)
triangular_index(1, 1)
triangular_index(2, 1)
triangular_index(1, 2)
triangular_index(2, 2)
*/
