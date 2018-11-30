#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
double max_dist_fun(arma::mat& points) {

    int nrows = points.n_rows;

    double  dist = 0,
        dist_temp = 0;

    for (int i = 0; i < nrows; i++){
        for(int j = 0; j < nrows; j++){

            dist_temp = std::sqrt(std::pow(points(i, 0) - points(j, 0), 2) +
                std::pow(points(i, 1) - points(j, 1), 2));

            if(dist_temp > dist) {
                dist = dist_temp;
            }
        }
    }
    return dist;
}

// [[Rcpp::export]]
arma::mat rcpp_get_circle(arma::mat points, double resolution_x, double resolution_y) {

    int     class_id= 0;

    arma::vec   id,
    unique_id;
    arma::uvec  temp_id;

    arma::mat   circle,
    points_temp,
    points_corner;

    id = points.col(2);
    unique_id = id(arma::find_unique(id));

    resolution_x = resolution_x / 2;
    resolution_y = resolution_y / 2;

    circle.set_size(unique_id.n_elem, 2);

    for(int i = 0; i < unique_id.n_elem; i++){

        class_id = unique_id(i);
        temp_id = arma::find(points.col(2) == class_id);
        points_temp = points.rows(temp_id);

        points_corner.set_size(points_temp.n_rows * 4, 2);

        int l = 0;

        for(int j = 0; j < points_temp.n_rows; j++){

            points_corner(l, 0) = points_temp(j, 0) - resolution_x;
            points_corner(l, 1) = points_temp(j, 1) - resolution_y;
            l++;

            points_corner(l, 0) = points_temp(j, 0) - resolution_x;
            points_corner(l, 1) = points_temp(j, 1) + resolution_y;
            l++;

            points_corner(l, 0) = points_temp(j, 0) + resolution_x;
            points_corner(l, 1) = points_temp(j, 1) + resolution_y;
            l++;

            points_corner(l, 0) = points_temp(j, 0) + resolution_x;
            points_corner(l, 1) = points_temp(j, 1) - resolution_y;
            l++;
        }

        circle(i, 0) = class_id;
        circle(i, 1) = std::pow((max_dist_fun(points_corner) / 2), 2) * arma::datum::pi;
    }

    return circle;
}

/*** R
mat <- matrix(c(
    x = c(21, 21, 22, 22),
    y = c(28, 29, 29, 28)),
    ncol = 2)

bench::mark(
    max(dist(mat)),
    max_dist_fun(mat),
    relative = TRUE,
    iterations = 100)

landscape_labeled <- get_patches(landscape,
                                 class = 1,
                                 directions = 8)[[1]]

points_class <- raster::rasterToPoints(landscape_labeled)

rcpp_get_circle(points_class,
                resolution_x = 1,
                resolution_y = 1)

*/
