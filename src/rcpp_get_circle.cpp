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

    unsigned  temp_id;

    NumericMatrix   circle,
                    points_temp,
                    points_corner;

    // get unique ID of points (patches)
    NumericVector y = points.column(1);
    NumericVector unique_id = unique(y);


    // matrix as long as patches
    circle(unique_id.length(), 2);

    // loop through all patches
    for(int i = 0; i < unique_id.length(); i++){

        // get patch ID of current point
        class_id = unique_id(i);

        // all points of current patch
        int n=points.nrow(), k=points.ncol();
        NumericMatrix out(sum(class_id),k);
        for (int i = 0, j = 0; i < n; i++) {
            if(class_id) {
                points_temp(j,_) = points(i,_);
                j = j+1;
            }
        }

        points_corner(points_temp.nrow() * 4, 2);

        int l = 0;

        for(int j = 0; j < points_temp.nrow(); j++){

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
