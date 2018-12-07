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
//' @keywords internal
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
        size_t row = std::fmod(c, n_rows);
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
//' @keywords internal
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
//' 4 (rook's case), 8 (queen's case), or a neighbourhood matrix.
//' The neighbourhood matrix should have one cell with value 0 (the focal cell),
//' and at least one cell with value 1 (the adjacent cells).
//' Cells with other values (e.g. NA) are ignored.
//' @keywords internal
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

/*** R
diagonal_matrix = matrix(c(1, NA, 1,
                            NA, 0, NA,
                            1, NA, 1), 3, 3, byrow = TRUE)
rcpp_create_neighborhood(diagonal_matrix)
*/
