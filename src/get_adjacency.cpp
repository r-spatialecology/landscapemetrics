#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Coordinates from a matrix
//'
//' This function gets coordinates (row and column numbers) of the matrix cells.
//'
//' @param x A matrix
//' @param cell A vector of cell number.
//' If NULL, the coordinates will be calculated for the whole matrix
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
        cells = seq(1, len) - 1;
    }
    // create a template two column matrix for a result
    IntegerMatrix result(len, 2);
    // for each cell...
    for (size_t i = 0; i < len; i++) {
        // ...get cell id
        int c = cells[i];
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

//' Create neighborhood coordinates
//'
//' This function creates a neighborhood coordinates matrix based on the directions parameter.
//'
//' @param directions The number of directions in which cells should be connected:
//' 4 (rook's case), 8 (queen's case), or a neigborhood matrix.
//' The neigborhood matrix should have one cell with value 0 (the focal cell),
//' and at least one cell with value 1 (the adjacent cells).
//' Cells with other values (e.g. NA) are ignored.
// [[Rcpp::export]]
IntegerMatrix rcpp_create_neighborhood(arma::imat directions){
    if (directions.n_elem == 1){
        int x = directions(0);
        IntegerVector x_id(x);
        IntegerVector y_id(x);
        if (x == 4){
            x_id = IntegerVector::create(0, -1, 1, 0);
            y_id = IntegerVector::create(-1, 0, 0, 1);
        } else if (x == 8){
            x_id = IntegerVector::create(-1, 0, 1, -1, 1, -1, 0, 1);
            y_id = IntegerVector::create(-1, -1, -1, 0, 0, 1, 1, 1);
        }
        IntegerMatrix neigh_coords(x_id.size(), 2);
        neigh_coords(_, 0) = x_id;
        neigh_coords(_, 1) = y_id;
        return neigh_coords;
    } else {
        IntegerVector center_position = as<IntegerVector>(wrap(find(directions == 0)));
        IntegerMatrix center_coords = rcpp_xy_from_matrix(directions, center_position);

        IntegerVector neigh_position = as<IntegerVector>(wrap(find(directions == 1)));
        IntegerMatrix neigh_coords = rcpp_xy_from_matrix(directions, neigh_position);

        neigh_coords(_,0) = neigh_coords(_,0) - center_coords(0, 0);
        neigh_coords(_,1) = neigh_coords(_,1) - center_coords(0, 1);

        return neigh_coords;
    }
}

//' Adjacent cells
//'
//' Identify cells that are adjacent to a set of cells on a matrix.
//' An output is a two columns matrix, where the first column contains a
//' main cell number and the second column contains an adjacent cell number.
//'
//' @param x A matrix
//' @param directions The number of directions in which cells should be connected:
//' 4 (rook's case), 8 (queen's case), or a neigborhood matrix.
//' The neigborhood matrix should have one cell with value 0 (the focal cell),
//' and at least one cell with value 1 (the adjacent cells).
//' Cells with other values (e.g. NA) are ignored.
// [[Rcpp::export]]
IntegerMatrix rcpp_get_adjacency(arma::imat x, arma::imat directions) {
    // extract coordinates from matrix
    IntegerMatrix xy = rcpp_xy_from_matrix(x);

    // get a number of rows
    int xy_nrows = xy.nrow();
    // create neighbots coordinates
    IntegerMatrix neigh_coords = rcpp_create_neighborhood(directions);
    int neigh_len = neigh_coords.nrow();
    // repeat neighbots coordinates
    IntegerMatrix neigh_coords_rep(neigh_len * xy_nrows, 2);
    neigh_coords_rep(_, 0) = rep_each(neigh_coords(_, 0), xy_nrows);
    neigh_coords_rep(_, 1) = rep_each(neigh_coords(_, 1), xy_nrows);

    // repreat center cells coordinates
    IntegerMatrix neighs(neigh_len * xy_nrows, 2);
    neighs(_, 0) = rep(as<IntegerVector>(wrap(xy(_, 0))), xy_nrows);
    neighs(_, 1) = rep(as<IntegerVector>(wrap(xy(_, 1))), xy_nrows);

    // move coordinates (aka get neighbors)
    neighs(_, 0) = neighs(_, 0) + neigh_coords_rep(_, 0);
    neighs(_, 1) = neighs(_, 1) + neigh_coords_rep(_, 1);

    // extract center cells cell numbers
    IntegerVector center_cells_unrep = rcpp_cell_from_xy(x, xy);
    // repeat center cells cell numbers
    IntegerVector center_cells(neigh_len * xy_nrows);
    center_cells = rep(center_cells_unrep, neigh_len);
    // extract neighbors cell numbers
    IntegerVector neighs_cells = rcpp_cell_from_xy(x, neighs);
    // combine the results
    IntegerMatrix result(center_cells.size(), 2);
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
//' 4 (rook's case), 8 (queen's case), or a neigborhood matrix.
//' The neigborhood matrix should have one cell with value 0 (the focal cell),
//' and at least one cell with value 1 (the adjacent cells).
//' Cells with other values (e.g. NA) are ignored.
// [[Rcpp::export]]
IntegerMatrix rcpp_get_pairs(arma::imat x, arma::imat directions) {
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

/*** R
mat = matrix(c(1, 1, 1, 2, 2, 1), ncol = 2)
diagonal_matrix = matrix(c(1, NA, 1,
                            NA, 0, NA,
                            1, NA, 1), 3, 3, byrow = TRUE)
rcpp_create_neighborhood(diagonal_matrix)
a = rcpp_get_adjacency(mat, directions = as.matrix(4))

b = landscapemetrics:::rcpp_get_adjacency(mat, directions = 4)

an = na.omit(a)
bn = na.omit(b)
*/
