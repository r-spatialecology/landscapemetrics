#include "rcpp_get_coocurrence_matrix.h"
#include "rcpp_create_neighborhood.h"
#include "rcpp_get_unique_values.h"
#include "get_class_index_map.h"

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix(const IntegerMatrix x,
                                           const arma::imat directions) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();

    std::vector<int> classes = rcpp_get_unique_values(x);
    std::map<int, unsigned> class_index = get_class_index_map(classes);

    unsigned n_classes = class_index.size();
    IntegerMatrix result(n_classes, n_classes);

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
                    result(focal_class,neig_class)++;
                }
            }
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
test <- landscapemetrics::augusta_nlcd
test <- raster("~/Desktop/lc_2008_4bit_clip.tif") # produces a matrix filled with NA ????
mat <- raster::as.matrix(test)
four <- as.matrix(4)
rcpp_get_coocurrence_matrix(mat, four)


lsm_p_contig(test)

rcpp_get_unique_values(mat)
*/
