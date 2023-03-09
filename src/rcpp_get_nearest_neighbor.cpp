#include <Rcpp.h>
using namespace Rcpp;

inline double compute_d2(double x1, double y1, double x2, double y2) {

    double dx = x2 - x1;
    double dy = y2 - y1;

    return dx * dx + dy * dy;
}

// [[Rcpp::export]]
NumericVector find_min(const NumericMatrix& points, int i, int m) {

    double x_i = points(i, 0), y_i = points(i, 1), id_i = points(i, 2);

    double x_k, x_min, x_max, d, d0 = R_PosInf;

    int k, id0;

    NumericVector dist_vec (2, 0.0);

    // Search before i
    x_min = R_NegInf;
    for (k = i - 1; k >= 0; k--) {
        if (points(k, 2) == id_i) continue;
        x_k = points(k, 0);
        if (x_k > x_min) {
            d = compute_d2(x_i, y_i, x_k, points(k, 1));
            if (d < d0) {
                d0 = d;
                id0 = points(k, 2);
                x_min = x_i - ::sqrt(d0);
            }
        } else {
            // No need to search further
            break;
        }
    }
    // Search after i
    x_max = R_PosInf;
    for (k = i + 1; k < m; k++) {
        if (points(k, 2) == id_i) continue;
        x_k = points(k, 0);
        if (x_k < x_max) {
            d = compute_d2(x_i, y_i, x_k, points(k, 1));
            if (d < d0) {
                d0 = d;
                id0 = points(k, 2);
                x_max = x_i + ::sqrt(d0);
            }
        } else {
            // No need to search further
            break;
        }
    }

    dist_vec(0) = ::sqrt(d0);
    dist_vec(1) = id0;

    return(dist_vec);
}

//' @title First nearest neighbor distance
//'
//' @description Efficiently calculate the distance to the first nearest neighbor.
//' Quasi linear runtime.
//'
//' @param points A two numeric matrix, where the first two columns are x and y
//'
//' @return Vector
//'
//' @author Florian Privé \email{florian.prive.21@gmail.com}
//'
//' @keywords internal
//'
//' @name rcpp_get_nearest_neighbor
//' @export
// [[Rcpp::export]]
NumericMatrix rcpp_get_nearest_neighbor(const NumericMatrix& points) {

    int nrows = points.nrow();
    NumericMatrix distances(nrows, 2);

    for (int i = 0; i < nrows; i++) {

        distances(i, _) = find_min(points, i, nrows);
    }

    return distances;
}


/*** R
landscape_labeled <- get_patches(landscape)

patches_class <- landscape_labeled[[1]][[1]]

class_boundaries <- terra::boundaries(patches_class, directions = 4, falseval = NA)

terra::values(class_boundaries)[terra::values(!is.na(class_boundaries))] <-
    terra::values(patches_class)[terra::values(!is.na(class_boundaries))]

points_class <- terra::xyFromCell(class_boundaries, cell = 1:terra::ncell(class_boundaries)) |>
    cbind(terra::values(class_boundaries, mat = FALSE)) |>
    stats::na.omit() |>
    tibble::as.tibble() |>
    purrr::set_names(c("x", "y", "id")) |>
    dplyr::arrange(id, -y)

X2 <- as.matrix(points_class)

res <- landscapemetrics:::rcpp_get_nearest_neighbor(X2)

find_closest <- function(X) {
    ord <- order(X[, 1])
    num <- seq_along(ord)
    rank <- match(num, ord)

    res <- rcpp_get_nearest_neighbor(X[ord,])

    unname(cbind(num, res[rank], X[, 3]))
    }

res2 <- find_closest(X2)

stopifnot(identical(res[, 2], res2[, 2]))

microbenchmark::microbenchmark(landscapemetrics:::rcpp_get_nearest_neighbor(X2),
                               find_closest(X2))
X3 <- X2[rep(seq_len(nrow(X2)), 50),]

microbenchmark::microbenchmark(landscapemetrics:::rcpp_get_nearest_neighbor(X3),
                               find_closest(X3),
                               times = 20)

stopifnot(identical(
    landscapemetrics:::rcpp_get_nearest_neighbor(X3),
    find_closest(X3)
))

# compute_d2(X2[1, 1], X2[1, 2], X2[1, 1], X2[1, 2])
# compute_d2(X2[2, 1], X2[2, 2], X2[1, 1], X2[1, 2])
    */
