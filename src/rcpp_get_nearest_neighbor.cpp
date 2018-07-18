#include <RcppArmadillo.h>

using namespace Rcpp;

double min_dist_fun(arma::mat point_a, arma::mat point_b) {

    double  dist = 0,
            dist_temp = 0;

    for (int i = 0; i < point_a.n_rows; i++){

        dist = std::sqrt(std::pow(point_a(i, 0) - point_b(0, 0), 2) +
            std::pow(point_a(i, 1) - point_b(0, 1), 2));

        for (int j = 1; j < point_b.n_rows; j++){

            dist_temp = std::sqrt(std::pow(point_a(i, 0) - point_b(j, 0), 2) +
                std::pow(point_a(i, 1) - point_b(j, 1), 2));

            if(dist_temp < dist) {
                dist = dist_temp;
                }
        }

    }

    return dist;
}

// [[Rcpp::export]]
arma::mat rcpp_get_nearest_neighbor(arma::mat points) {

    int nrows = points.n_rows;
    int ncols = 3;
    int id = 0;

    arma::mat points_temp;
    arma::uvec keep_id;

    arma::mat distance_mat(nrows, ncols);

    // Fill with value
    for (int i = 0; i < nrows; i++) {

        points_temp = points;
        points_temp.shed_row(i);

        id = points(i, 2);

        keep_id = find(points_temp.col(2) != points(i, 2));

        points_temp = points_temp.rows(keep_id);

        distance_mat(i, 0) = i + 1;
        distance_mat(i, 1) = min_dist_fun(points.row(i), points_temp);
        distance_mat(i, 2) = id;

    }


    return distance_mat;
}

/*** R
landscape_labelled <- cclabel(landscape)

patches_class <- landscape_labelled[[1]]

class_boundaries <- raster::boundaries(patches_class, directions = 4,
                                       asNA = TRUE)

raster::values(class_boundaries)[raster::values(!is.na(class_boundaries))] <-
    raster::values(patches_class)[raster::values(!is.na(class_boundaries))]

points_class <- raster::xyFromCell(class_boundaries,
                                   cell = 1:raster::ncell(class_boundaries)) %>%
    cbind(raster::values(class_boundaries)) %>%
    stats::na.omit() %>%
    tibble::as.tibble() %>%
    purrr::set_names(c("x", "y", "id"))  %>%
    dplyr::arrange(id, -y)

min_dist <- rcpp_get_nearest_neighbor(as.matrix(points_class))

*/
