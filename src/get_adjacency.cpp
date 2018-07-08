#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
IntegerMatrix rcpp_xy_from_matrix(arma::imat x) {

    int n_rows = x.n_rows;
    int n_cols = x.n_cols;

    size_t len = n_rows * n_cols;
    IntegerVector cell = seq(1, len);

    IntegerMatrix result(len, 2);

    for (size_t i = 0; i < len; i++) {
        int c = cell[i] - 1;
        size_t col = fmod(c, n_cols);
        size_t row = (c / n_cols);
        result(i, 0) = col;
        result(i, 1) = row;
    }

    return result;
}

// [[Rcpp::export]]
IntegerVector rcpp_cell_from_xy(arma::imat x, IntegerMatrix y) {

    size_t len = y.nrow();

    int n_rows = x.n_rows;
    int n_cols = x.n_cols;

    IntegerVector result(len);

    for (size_t i = 0; i < len; i++) {
        double row = y(i, 1);
        double col = y(i, 0);

        if (row < 0 || row >= n_rows || col < 0 || col >= n_cols) {
            result[i] = NA_INTEGER;
        } else {
            result[i] = row * n_cols + col + 1 ;
        }
    }

    return result;
}


// [[Rcpp::export]]
IntegerMatrix rcpp_get_adjacency_xy(arma::imat x) {

    IntegerMatrix xy = rcpp_xy_from_matrix(x);

    IntegerVector row_ids = wrap(xy(_, 0));
    IntegerVector col_ids = wrap(xy(_, 1));

    int row_ids_size = row_ids.size();
    int col_ids_size = col_ids.size();

    // initiate neighbors ids
    IntegerVector x_id = IntegerVector::create(0, -1, 1, 0);
    IntegerVector y_id = IntegerVector::create(-1, 0, 0, 1);

    IntegerVector row_ids_rep(x_id.size() * row_ids_size);
    row_ids_rep = rep(row_ids, x_id.size());

    IntegerVector col_ids_rep(y_id.size() * col_ids_size);
    col_ids_rep = rep(col_ids, y_id.size());

    IntegerVector x_id_rep(x_id.size() * row_ids_size);
    x_id_rep = rep_each(x_id, row_ids_size);

    IntegerVector y_id_rep(y_id.size() * col_ids_size);
    y_id_rep = rep_each(y_id, col_ids_size);

    row_ids_rep = row_ids_rep + x_id_rep;
    col_ids_rep = col_ids_rep + y_id_rep;

    IntegerMatrix result(row_ids_rep.size(), 2);
    result(_, 0) = row_ids_rep;
    result(_, 1) = col_ids_rep;

    int n_elem_size = x.n_elem;
    IntegerVector n_elem_rep(x_id.size() * n_elem_size);

    IntegerVector n_elem = seq_len(n_elem_size) - 1;
    n_elem_rep = rep(n_elem, x_id.size());

    return result;
}

/*** R
x = matrix(c(1, 1, 1, 2), ncol = 2)
x
y = rcpp_xy_from_matrix(x)
y
z = rcpp_cell_from_xy(x, y)
z
rcpp_get_adjacency_xy(x)
*/
