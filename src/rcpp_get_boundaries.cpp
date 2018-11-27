#include "rcpp_get_boundaries.h"
#include "rcpp_create_neighborhood.h"
#include "rcpp_get_unique_values.h"
#include "get_class_index_map.h"

// [[Rcpp::export]]
IntegerMatrix rcpp_get_boundaries(const IntegerMatrix x,
                            const arma::imat directions) {
    const int na = NA_INTEGER;
    const unsigned ncols = x.ncol();
    const unsigned nrows = x.nrow();

    std::vector<int> classes = rcpp_get_unique_values(x);
    std::map<int, unsigned> class_index = get_class_index_map(classes);

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

    IntegerMatrix boundary(nrows, ncols);

    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int tmp = x[col * nrows + row];
            if (tmp == na)
                continue;
            unsigned focal_class = class_index[tmp];
            boundary(row, col) = 0;
            for (int h = 0; h < neigh_len; h++) {
                int neig_col = neig_coords[h][0] + col;
                int neig_row = neig_coords[h][1] + row;
                if (neig_col >= 0 &&
                    neig_row >= 0 &&
                    neig_col < ncols &&
                    neig_row < nrows) {
                    const int tmp = x[neig_col * nrows + neig_row];
                    const unsigned neig_class = class_index[tmp];
                    if (neig_class != focal_class) {
                        boundary(row, col) = 1;
                        break;
                    }
                }
            }
        }
    }

    return boundary;
}
/*** R
test <- landscapemetrics::get_patches(landscapemetrics::landscape, class = 1)[[1]]
raster::plot(test)
landscapetools::util_plot(raster::boundaries(test))


boundarie_mat <- rcpp_get_boundaries(raster::as.matrix(pad_raster(test, pad_raster_value = NA)), as.matrix(4))

landscapetools::util_plot(raster::raster(boundarie_mat))

*/
