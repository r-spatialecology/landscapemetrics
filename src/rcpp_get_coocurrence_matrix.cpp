#include "get_coocurrence_matrix.h"
#include "get_adjacency.h"

IntegerMatrix rcpp_get_coocurrence_matrix(arma::imat x, arma::imat directions) {

    // get unique values
    arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));

    // create a matrix of zeros of unique values size
    arma::imat cooc_mat(u.n_elem, u.n_elem, arma::fill::zeros);

    // extract adjency pairs
    IntegerMatrix pairs = rcpp_get_pairs(x, directions);

    IntegerVector center_cells = pairs[0];
    IntegerVector neigh_cells = pairs[1];

    // number of pairs
    int num_pairs = center_cells.length();

    // for each row and col
    for (int i = 0; i < num_pairs; i++) {

        // extract value of central cell and its neighbot
        int center = center_cells(i);
        int neigh = neigh_cells(i);

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

IntegerMatrix rcpp_get_coocurrence_matrix2(const IntegerMatrix &x,
                                           const arma::imat &directions) {
    int min = Rcpp::min(x);
    int max = Rcpp::max(x);
    unsigned ncols = x.ncol();
    unsigned nrows = x.nrow();
    unsigned n_classes = 1 + max - min;
    std::vector<std::vector<unsigned> > cooc_mat(n_classes,
                                                std::vector<unsigned>(n_classes));

    const std::vector<std::vector<int> > neig_coords = {{-1, 0}, {0, -1}, {0, 1}, {1, 0}};

    // auto neig_coords_ = rcpp_create_neighborhood(directions);
//    std::vector<std::vector<int>> neig_coords(neig_coords_.nrow(),
//                                              std::vector<int>(neig_coords_.ncol()));
//    for (unsigned col = 0; col < neig_coords_.ncol(); col++) {
//        for (unsigned row = 0; row < neig_coords_.nrow(); row++) {
//            neig_coords[col][row] = neig_coords_(col, row);
//        }
//    }

//    std::vector<std::vector<int> > landscape(ncols, std::vector<int>(nrows));
//    for (unsigned col = 0; col < ncols; col++) {
//        for (unsigned row = 0; row < nrows; row++) {
//            landscape[col][row] = x[col * nrows + row];
//        }
//    }

    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            unsigned focal_class = x[col * nrows + row] - min;
            for (auto neig : neig_coords) {
                int neig_col = neig[0] + col;
                int neig_row = neig[1] + row;
                if (neig_col >= 0 &&
                        neig_row >= 0 &&
                        neig_col < ncols &&
                        neig_row < nrows) {
                    unsigned neig_class = x[neig_col * nrows + neig_row] - min;
                        cooc_mat[focal_class][neig_class]++;
                }
            }
        }
    }

    IntegerMatrix result(n_classes, n_classes);
    for (unsigned col = 0; col < cooc_mat.size(); col++) {
        for (unsigned row = 0; row < cooc_mat[col].size(); row++) {
            result(col, row) = cooc_mat[col][row];
        }
    }

    return result;
}


/*** R

library(raster)
library(dplyr)
test <- landscapemetrics::landscape
test <- raster("~/Downloads/lc_2008_4bit_clip.tif") # produces a matrix filled with NA ????
mat <- raster::as.matrix(test)
four <- as.matrix(4)
test <- landscapemetrics:::rcpp_get_coocurrence_matrix2(mat, four)
*/
