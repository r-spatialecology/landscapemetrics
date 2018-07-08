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
            // result[i] = row * n_cols + col + 1 ;
            result[i] = row * n_cols + col;
        }
    }

    return result;
}


// [[Rcpp::export]]
IntegerMatrix rcpp_get_adjacency(arma::imat x, int directions) {

    IntegerMatrix xy = rcpp_xy_from_matrix(x);

    IntegerVector row_ids = wrap(xy(_, 0));
    IntegerVector col_ids = wrap(xy(_, 1));

    int row_ids_size = row_ids.size();
    int col_ids_size = col_ids.size();

    IntegerVector x_id(directions);
    IntegerVector y_id(directions);

    if (directions == 4){
        // initiate neighbors ids
        x_id = IntegerVector::create(0, -1, 1, 0);
        y_id = IntegerVector::create(-1, 0, 0, 1);
    }

    IntegerVector row_ids_rep(directions * row_ids_size);
    row_ids_rep = rep(row_ids, directions);

    IntegerVector col_ids_rep(directions * col_ids_size);
    col_ids_rep = rep(col_ids, directions);

    IntegerVector x_id_rep(directions * row_ids_size);
    x_id_rep = rep_each(x_id, row_ids_size);

    IntegerVector y_id_rep(directions * col_ids_size);
    y_id_rep = rep_each(y_id, col_ids_size);

    row_ids_rep = row_ids_rep + x_id_rep;
    col_ids_rep = col_ids_rep + y_id_rep;

    IntegerMatrix neighs(row_ids_rep.size(), 2);
    neighs(_, 0) = row_ids_rep;
    neighs(_, 1) = col_ids_rep;

    int n_elem_size = x.n_elem;
    IntegerVector n_elem = seq_len(n_elem_size) - 1;
    IntegerVector center_cells(directions * n_elem_size);

    center_cells = rep(n_elem, directions);
    IntegerVector neighs_cells = rcpp_cell_from_xy(x, neighs);

    IntegerMatrix result(row_ids_rep.size(), 2);
    result(_, 0) = center_cells;
    result(_, 1) = neighs_cells;

    return result;
}

// [[Rcpp::export]]
NumericMatrix rcpp_get_coocurrence_matrix2(arma::imat x, int directions = 4) {

    // get unique values
    arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));
    // create a matrix of zeros of unique values size
    arma::imat cooc_mat(u.n_elem, u.n_elem, arma::fill::zeros);

    IntegerMatrix adjency_pairs = rcpp_get_adjacency(x, directions);
    // number of rows and cols
    int num_pairs = adjency_pairs.nrow();
    // Rcpp::Rcout << num_pairs << std::endl;

    // for each row and col
    for (int i = 0; i < num_pairs; i++) {
        int neigh_cell = adjency_pairs(i, 1);
        // Rcpp::Rcout << neigh_cell << std::endl;

        if (neigh_cell != INT_MIN){
            int center = x(adjency_pairs(i, 0));
            int neigh = x(neigh_cell);
            arma::uvec loc_c = find(u == center);
            arma::uvec loc_n = find(u == neigh);
            cooc_mat(loc_c, loc_n) += 1;
        }
    }
    // return a coocurence matrix
    NumericMatrix cooc_mat_result = as<NumericMatrix>(wrap(cooc_mat));
    // add names
    List u_names = List::create(u, u);
    cooc_mat_result.attr("dimnames") = u_names;
    return cooc_mat_result;
}

/*** R
x = matrix(c(1, 1, 1, 2), ncol = 2)
x
y = rcpp_xy_from_matrix(x)
y
z = rcpp_cell_from_xy(x, y)
z
a = rcpp_get_adjacency(x, 4)
na.omit(a)
b = rcpp_get_coocurrence_matrix2(x, 4)
b
*/
