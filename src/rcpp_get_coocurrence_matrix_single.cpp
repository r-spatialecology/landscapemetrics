#include "rcpp_get_coocurrence_matrix_single.h"
#include "rcpp_create_neighborhood.h"
#include "rcpp_get_unique_values.h"
#include "rcpp_get_class_index_map.h"

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix_single(const IntegerMatrix x,
                                           const arma::imat directions,
                                           const int single_class) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();

    std::vector<int> classes = rcpp_get_unique_values(x);
    std::map<int, unsigned> class_index = get_class_index_map(classes);

    unsigned n_classes = class_index.size();
    IntegerMatrix cooc_mat(n_classes, 1);

    // create neighbors coordinates
    IntegerMatrix tmp = rcpp_create_neighborhood(directions);
    int neigh_len = tmp.nrow();
    std::vector<std::vector<int> > neig_coords;
    for (int row = 0; row < neigh_len; row++) {
        IntegerVector a = tmp.row(row);
        std::vector<int> b(a.begin(), a.end());
        neig_coords.push_back(b);
    }

    // NAs need an index, otherwise they are counted as neighbors of class[0]
    class_index.insert(std::make_pair(na, n_classes));

    int single_class_index = class_index[single_class];

    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int tmp = x[col * nrows + row];
            if (tmp == na)
              continue;
            int focal_class = class_index[tmp];
            if (focal_class != single_class_index)
              continue;
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
                    cooc_mat(neig_class,focal_class)++;
                }
            }
        }
    }

    // add names
    List u_names = List::create(classes, single_class);
    cooc_mat.attr("dimnames") = u_names;
    return cooc_mat;
}

/*** R
library(terra)
library(dplyr)
test <- terra::unwrap(landscapemetrics::augusta_nlcd)
# test <- raster("~/Desktop/lc_2008_4bit_clip.tif") # produces a matrix filled with NA ????
mat <- terra::as.matrix(test, wide = TRUE)
four <- as.matrix(4)
rcpp_get_coocurrence_matrix(mat, four)
rcpp_get_coocurrence_matrix_single(mat, four, 11)
*/
