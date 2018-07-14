#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Coordinates from a matrix
//'
//' These functions get coordinates (row and column numbers) of the matrix cells.
//'
//' @param x A matrix
// [[Rcpp::export]]
IntegerMatrix rcpp_xy_from_matrix(arma::imat x, Rcpp::Nullable<Rcpp::IntegerVector> cell = R_NilValue) {
    // adapted from raster::xyFromCell()
    // get number of rows and columns
    int n_rows = x.n_rows;
    int n_cols = x.n_cols;
    // init objects
    size_t len;
    IntegerVector cells;

    if (cell.isNotNull()){
        // calculate only for selected cells
        // create a vector with cells ids
        cells = IntegerVector(cell);
        // calculate a number of cells
        len = cells.size();
    } else {
        // calculate the whole matrix
        // calculate a number of cells
        len = n_rows * n_cols;
        // create a vector with cells ids
        cells = seq(1, len);
    }
    // create a template two column matrix for a result
    IntegerMatrix result(len, 2);
    // for each cell...
    for (size_t i = 0; i < len; i++) {
        // ...get cell id
        int c = cells[i] - 1;
        // ...get column number
        size_t col = (c / n_rows);
        // ...get row number
        size_t row = fmod(c, n_rows);
        // ...insert cols and rows
        result(i, 1) = col;
        result(i, 0) = row;
    }
    return result;
}

//' Get cell number
//'
//' Get cell number(s) of a matrix from row and column numbers.
//' Cell numbers start at 0 in the upper left corner,
//' and increase from top to bottom, and then from left to right.
//'
//' @param x A matrix
//' @param y A matrix with two columns (row and column numbers)
// [[Rcpp::export]]
IntegerVector rcpp_cell_from_xy(arma::imat x, IntegerMatrix y) {
    // adapted from raster::cellFromXY()
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

//' Adjacent cells
//'
//' Identify cells that are adjacent to a set of cells on a matrix.
//' An output is a two columns matrix, where the first column contains a
//' main cell number and the second column contains an adjacent cell number.
//'
//' @param x A matrix
//' @param directions The number of directions in which cells should be connected:
//' 4 (rook's case) or 8 (queen's case)
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

//' Adjacent cells values
//'
//' Extract values of adjacent cells.
//' An output is a two columns matrix, where the first column contains a
//' main cell value and the second column contains an adjacent cell value.
//'
//' @param x A matrix
//' @param directions The number of directions in which cells should be connected:
//' 4 (rook's case) or 8 (queen's case)
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
