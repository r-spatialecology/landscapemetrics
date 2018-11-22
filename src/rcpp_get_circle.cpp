#include "rcpp_get_circle.h"
#include "rcpp_get_unique_values.h"

double max_dist_fun(NumericMatrix points) {

    int nrows = points.nrow();

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
NumericMatrix rcpp_get_circle(NumericMatrix points, double resolution_x, double resolution_y) {

    int     class_id = 0;

    NumericVector   id,
                    unique_id;

    unsigned  temp_id;

    NumericMatrix   circle,
                    points_temp,
                    points_corner;

    // get unique ID of points (patches)
    NumericMatrix y = points(1,1);
    unique_id = rcpp_get_unique_values();

    // matrix as long as patches
    circle(unique_id.length(), 2);

    // loop through all patches
    for(int i = 0; i < unique_id.length(); i++){

        // get patch ID of current point
        class_id = unique_id(i);

        // all points of current patch
        points_temp = points[points(_,1) == class_id];

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
landscape_labeled <- get_patches(landscape, class = 1)

    patches_class <- landscape_labeled[[1]]

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
