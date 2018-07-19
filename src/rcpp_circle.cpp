#include <RcppArmadillo.h>

using namespace Rcpp;

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
arma::mat rcpp_get_circle(arma::mat points, double resolution) {

    int     class_id= 0;

    arma::vec   id,
                unique_id;
    arma::uvec  temp_id;

    arma::mat   circle,
                points_temp,
                points_corner;

    id = points.col(2);
    unique_id = id(arma::find_unique(id));

    circle.set_size(unique_id.n_elem, 2);

    for(int i = 0; i < unique_id.n_elem; i++){

        class_id = unique_id(i);
        temp_id = arma::find(points.col(2) == class_id);
        points_temp = points.rows(temp_id);

        points_corner.set_size(points_temp.n_rows * 4, 2);

        int l = 0;

        for(int j = 0; j < points_temp.n_rows; j++){

            points_corner(l, 0) = points_temp(j, 0) - resolution;
            points_corner(l, 1) = points_temp(j, 1) - resolution;
            l++;

            points_corner(l, 0) = points_temp(j, 0) - resolution;
            points_corner(l, 1) = points_temp(j, 1) + resolution;
            l++;

            points_corner(l, 0) = points_temp(j, 0) + resolution;
            points_corner(l, 1) = points_temp(j, 1) + resolution;
            l++;

            points_corner(l, 0) = points_temp(j, 0) + resolution;
            points_corner(l, 1) = points_temp(j, 1) - resolution;
            l++;
        }

        circle(i, 0) = class_id;
        circle(i, 1) = std::pow((max_dist_fun(points_corner) / 2), 2) * arma::datum::pi;


    }

    return circle;
}

/*** R
landscape_labelled <- cclabel(landscape)

    patches_class <- landscape_labelled[[1]]

class_boundaries <-
    raster::boundaries(patches_class, directions = 4,
                       asNA = TRUE)

    raster::values(class_boundaries)[raster::values(!is.na(class_boundaries))] <-
        raster::values(patches_class)[raster::values(!is.na(class_boundaries))]

points_class <- raster::xyFromCell(class_boundaries,
                                   cell = 1:raster::ncell(class_boundaries)) %>%
cbind(raster::values(class_boundaries)) %>%
stats::na.omit() %>%
tibble::as.tibble() %>%
purrr::set_names(c("x", "y", "id"))  %>%
dplyr::arrange(id,-y)

# mat <- matrix(c(
#     x = c(21, 21, 22, 22),
#     y = c(28, 29, 29, 28)),
#     ncol = 2)
#
# max(stats::dist(mat))
#
# max_dist_fun(mat)
#
# max_dist <- max_dist_fun(as.matrix(points_class[, 1:2]))

x <- rcpp_get_circle(as.matrix(points_class), resolution = 1)

*/
