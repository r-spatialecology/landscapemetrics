#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
IntegerMatrix rcpp_xy_from_matrix(arma::imat x) {
    // get number of rows and columns
    int n_rows = x.n_rows;
    int n_cols = x.n_cols;
    // calculate a number of cells
    size_t len = n_rows * n_cols;
    // create a vector with cells ids
    IntegerVector cell = seq(1, len);
    // create a template two column matrix for a result
    IntegerMatrix result(len, 2);
    // for each cell...
    for (size_t i = 0; i < len; i++) {
        // ...get cell id
        int c = cell[i] - 1;
        // ...get column number
        size_t col = fmod(c, n_cols);
        // ...get row number
        size_t row = (c / n_cols);
        // ...insert cols and rows
        result(i, 1) = col;
        result(i, 0) = row;
    }
    return result;
}

// [[Rcpp::export]]
IntegerVector rcpp_cell_from_xy(arma::imat x, IntegerMatrix y) {
    // get number of rows and columns
    int n_rows = x.n_rows;
    int n_cols = x.n_cols;
    // get length of the query
    size_t len = y.nrow();
    IntegerVector result(len);
    // for each query...
    for (size_t i = 0; i < len; i++) {
        // extract column and row of the query
        double col = y(i, 1);
        double row = y(i, 0);
        // calculate cell numbers (NA if outside of the input matrix)
        if (row < 0 || row >= n_rows || col < 0 || col >= n_cols) {
            result[i] = NA_INTEGER;
        } else {
            result[i] = col * n_rows + row;
        }
    }
    return result;
}

// [[Rcpp::export]]
IntegerMatrix rcpp_get_adjacency(arma::imat x, int directions) {
    // extract coordinates from matrix
    IntegerMatrix xy = rcpp_xy_from_matrix(x);
    // get a number of rows
    int xy_nrows = xy.nrow();
    // initiate neighbors ids
    IntegerVector x_id(directions);
    IntegerVector y_id(directions);
    if (directions == 4){
        x_id = IntegerVector::create(0, -1, 1, 0);
        y_id = IntegerVector::create(-1, 0, 0, 1);
    } else if (directions == 8){
        x_id = IntegerVector::create(-1, 0, 1, -1, 1, -1, 0, 1);
        y_id = IntegerVector::create(-1, -1, -1, 0, 0, 1, 1, 1);
    }
    // replicate directions vectors
    IntegerVector x_id_rep(directions * xy_nrows);
    x_id_rep = rep_each(x_id, xy_nrows);
    IntegerVector y_id_rep(directions * xy_nrows);
    y_id_rep = rep_each(y_id, xy_nrows);
    // get x and y coordinates
    IntegerVector row_ids = wrap(xy(_, 0));
    IntegerVector col_ids = wrap(xy(_, 1));
    // replicate coordinates vectors
    IntegerVector row_ids_rep(directions * xy_nrows);
    row_ids_rep = rep(row_ids, directions);
    IntegerVector col_ids_rep(directions * xy_nrows);
    col_ids_rep = rep(col_ids, directions);
    // move coordinates (aka get neighbors)
    row_ids_rep = row_ids_rep + x_id_rep;
    col_ids_rep = col_ids_rep + y_id_rep;
    // create neighbors matrix
    IntegerMatrix neighs(row_ids_rep.size(), 2);
    neighs(_, 0) = row_ids_rep;
    neighs(_, 1) = col_ids_rep;
    // extract center cells cell numbers
    IntegerVector center_cells_unrep = rcpp_cell_from_xy(x, xy);
    // repeat center cells cell numbers
    IntegerVector center_cells(directions * xy_nrows);
    center_cells = rep(center_cells_unrep, directions);
    // extract neighbors cell numbers
    IntegerVector neighs_cells = rcpp_cell_from_xy(x, neighs);
    // combine the results
    IntegerMatrix result(row_ids_rep.size(), 2);
    result(_, 0) = center_cells;
    result(_, 1) = neighs_cells;
    return result;
}

// [[Rcpp::export]]
IntegerMatrix rcpp_get_pairs(arma::imat x, int directions = 4) {
    // extract adjency pairs
    IntegerMatrix adjency_pairs = rcpp_get_adjacency(x, directions);
    // number of pairs
    int num_pairs = adjency_pairs.nrow();
    // result template matrix
    IntegerMatrix result(num_pairs, 2);
    // for each pair...
    for (int i = 0; i < num_pairs; i++) {
        // get neighbor and central cell
        int neigh_cell = adjency_pairs(i, 1);
        int center_cell = adjency_pairs(i, 0);
        // if neighbor exist then input values
        if (neigh_cell != INT_MIN){
            result(i, 0) = x(center_cell);
            result(i, 1) = x(neigh_cell);
        } else {
            result(i, 0) = x(center_cell);
            result(i, 1) = NA_INTEGER;
        }
    }
    return result;
}

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix2(arma::imat x, int directions = 4) {
    // get unique values
    arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));
    // create a matrix of zeros of unique values size
    arma::imat cooc_mat(u.n_elem, u.n_elem, arma::fill::zeros);
    // extract adjency pairs
    IntegerMatrix pairs = rcpp_get_pairs(x, directions);
    // number of pairs
    int num_pairs = pairs.nrow();
    // for each row and col
    for (int i = 0; i < num_pairs; i++) {
        // extract value of central cell and its neighbot
        int center = pairs(i, 0);
        int neigh = pairs(i, 1);
        // find their location in the output matrix
        arma::uvec loc_c = find(u == center);
        arma::uvec loc_n = find(u == neigh);
        // add its count
        cooc_mat(loc_c, loc_n) += 1;
    }
    // return a coocurence matrix
    IntegerMatrix cooc_mat_result = as<IntegerMatrix>(wrap(cooc_mat));
    // add names
    List u_names = List::create(u, u);
    cooc_mat_result.attr("dimnames") = u_names;
    return cooc_mat_result;
}

/*** R
x = matrix(c(1, 1, 1, 2, 2, 1), ncol = 2)
x
y = rcpp_xy_from_matrix(x)
y
z = rcpp_cell_from_xy(x, y)
z
a = rcpp_get_adjacency(x, 4)
na.omit(a)
rcpp_get_pairs(x, 4)

b = rcpp_get_coocurrence_matrix2(x, 4)
b
# 1 2
# 1 6 3
# 2 3 2
d = rcpp_get_coocurrence_matrix2(land, 4)
d
rcpp_get_coocurrence_matrix2(raster::as.matrix(landscape), directions = 4)
#      -999   1   2    3
# -999  248  16  37   67
# 1      16 520  43  137
# 2      37  43 704  184
# 3      67 137 184 1528
*/
