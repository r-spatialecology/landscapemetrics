#include "get_coocurrence_matrix.h"
#include "get_adjacency.h"

IntegerMatrix rcpp_get_coocurrence_matrix(arma::imat x, arma::imat directions) {
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

bool myfunction (int i, int j) {
    return (i==j);
}

IntegerMatrix rcpp_get_coocurrence_matrix2(const IntegerMatrix x,
                                           const arma::imat directions,
                                           IntegerVector classes) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();
    std::map<int, unsigned> class_index;
    for (unsigned i = 0; i < classes.size(); i++) {
        if (classes[i] == na)
            continue;
        class_index.insert(std::make_pair(classes[i], i));
    }
    unsigned n_classes = classes.size();
    std::vector<std::vector<unsigned> > cooc_mat(n_classes,
                                                 std::vector<unsigned>(n_classes));

    const std::vector<std::vector<int> > neig_coords = {{-1, 0}, {0, -1}, {0, 1}, {1, 0}};

    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int tmp = x[col * nrows + row];
            if (tmp == na)
                continue;
            unsigned focal_class = class_index[tmp];
            for (auto neig : neig_coords) {
                int neig_col = neig[0] + col;
                int neig_row = neig[1] + row;
                if (neig_col >= 0 &&
                        neig_row >= 0 &&
                        neig_col < ncols &&
                        neig_row < nrows) {
                    const int tmp = x[neig_col * nrows + neig_row];
                    if (tmp == na)
                        continue;
                    unsigned neig_class = class_index[tmp];
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

    // add names
    List u_names = List::create(classes, classes);
    result.attr("dimnames") = u_names;
    return result;
}


/*** R

library(raster)
library(dplyr)
test <- landscapemetrics::landscape
test <- raster("~/Downloads/lc_2008_4bit_clip.tif") # produces a matrix filled with NA ????
mat <- raster::as.matrix(test)
four <- as.matrix(4)
test0 <- microbenchmark::microbenchmark(
    res1 <- landscapemetrics:::lsm_get_coocurrence_matrix_(mat, four, ncol, nrow, classes), unit = "s", times = 1) # 25s
test0_2 <- microbenchmark::microbenchmark(
    res1 <- landscapemetrics:::lsm_get_coocurrence_matrix(test, four), unit = "s", times = 1) # 80s
res2 <- landscapemetrics:::rcpp_get_coocurrence_matrix(mat, four)
test <- microbenchmark::microbenchmark(landscapemetrics:::lsm_get_coocurrence_matrix(test, four), unit = "ms")
test2 <- microbenchmark::microbenchmark(landscapemetrics:::rcpp_get_coocurrence_matrix(mat, four), unit = "ms")
summary(test2)
    */
