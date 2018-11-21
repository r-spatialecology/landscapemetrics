#include "rcpp_get_coocurrence_matrix.h"
#include "rcpp_create_neighborhood.h"
#include "lsm_unique.h"

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix(const IntegerMatrix x,
                                           const arma::imat directions) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();

    std::vector<int> classes = lsm_unique(x);
    std::map<int, unsigned> class_index = get_classes_map(classes);

    unsigned n_classes = class_index.size();
    std::vector<std::vector<unsigned> > cooc_mat(n_classes,
                                                 std::vector<unsigned>(n_classes));

    // create neighbors coordinates
    IntegerMatrix tmp = rcpp_create_neighborhood(directions);
    int neigh_len = tmp.nrow();
    std::vector<std::vector<int> > neig_coords;
    for (int row = 0; row < neigh_len; row++) {
        IntegerVector a = tmp.row(row);
        std::vector<int> b(a.begin(), a.end());
        neig_coords.push_back(b);
    }

    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int tmp = x[col * nrows + row];
            if (tmp == na)
                continue;
            unsigned focal_class = class_index[tmp];
            for (int h = 0; h < neigh_len; h++) {
                int neig_col = neig_coords[h][0] + col;
                int neig_row = neig_coords[h][1] + row;
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

std::map<int, unsigned> get_classes_map(const std::vector<int> &classes)
{
    std::map<int, unsigned> class_index;
    for (unsigned i = 0; i < classes.size(); i++) {
        class_index.insert(std::make_pair(classes[i], i));
    }
    return class_index;
}


/*** R

library(raster)
library(dplyr)
test <- landscapemetrics::augusta_nlcd
test <- raster("~/Desktop/lc_2008_4bit_clip.tif") # produces a matrix filled with NA ????
mat <- raster::as.matrix(test)
four <- as.matrix(4)
rcpp_get_coocurrence_matrix(mat, four)

lsm_unique(mat)
*/
