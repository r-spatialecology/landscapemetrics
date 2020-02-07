#include "rcpp_get_circle.h"
#include "get_class_index_map.h"
#include "rcpp_get_unique_values.h"
#include "smallest_circle.h"

DataFrame rcpp_get_circle(IntegerMatrix mat,
                          const unsigned resolution_xy) {

    // create index-patch_id map
    std::vector<int> patch_id = rcpp_get_unique_values(mat);
    auto patch_id_index = get_class_index_map(patch_id);
    const auto n_patches = patch_id_index.size();

    const unsigned ncols = mat.ncol();
    const unsigned nrows = mat.nrow();

    std::vector<unsigned> patch_area(n_patches);
    std::vector<unsigned> patch_height(n_patches);
    std::vector<unsigned> patch_width(n_patches);

    std::vector<Circle> circles(n_patches);
    std::vector<double> circle_area(n_patches);
    std::vector<double> circle_center_x(n_patches);
    std::vector<double> circle_center_y(n_patches);
    std::vector<double> circle_diameter(n_patches);

    std::vector<unsigned> col_min(n_patches, ncols);
    std::vector<unsigned> col_min_y(n_patches, 0);
    std::vector<unsigned> row_min_x(n_patches, 0);
    std::vector<unsigned> row_min(n_patches, nrows);

    std::vector<unsigned> col_max(n_patches, 0);
    std::vector<unsigned> col_max_y(n_patches, 0);
    std::vector<unsigned> row_max_x(n_patches, 0);
    std::vector<unsigned> row_max(n_patches, 0);

    // calculate patch area and the spatial extent of each patch
    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int class_id = mat[col * nrows + row];

            if (class_id == NA_INTEGER) continue;

            const unsigned class_idx = patch_id_index[class_id];

            if (row < row_min[class_idx]) {
                row_min[class_idx] = row;
            }
            if (row > row_max[class_idx]) {
                row_max[class_idx] = row;
            }
            if (col < col_min[class_idx]) {
                col_min[class_idx] = col;
            }
            if (col > col_max[class_idx]) {
                col_max[class_idx] = col;
            }
            patch_area[class_idx]++;
        }
    }

    // find significant border points
    std::vector<std::vector<Point> > border_points(n_patches);
    for (unsigned col = 0; col < ncols; col++) {
        for (unsigned row = 0; row < nrows; row++) {
            const int class_id = mat[col * nrows + row];
            if (class_id == NA_INTEGER) continue;
            const unsigned class_idx = patch_id_index[class_id];

            const double col_ = col * resolution_xy;
            const double row_ = row * resolution_xy;

            if (col == col_max[class_idx]) {
                border_points[class_idx].push_back(Point(col_ + resolution_xy, row_));
                border_points[class_idx].push_back(Point(col_ + resolution_xy, row_ + resolution_xy));
            }

            if (col == col_min[class_idx]) {
                border_points[class_idx].push_back(Point(col_, row_));
                border_points[class_idx].push_back(Point(col_, row_ + resolution_xy));
            }

            if (row == row_max[class_idx]) {
                border_points[class_idx].push_back(Point(col_, row_ + resolution_xy));
                border_points[class_idx].push_back(Point(col_ + resolution_xy, row_ + resolution_xy));
            }

            if (row == row_min[class_idx]) {
                border_points[class_idx].push_back(Point(col_, row_));
                border_points[class_idx].push_back(Point(col_ + resolution_xy, row_));
            }
        }
    }

    // calculate smallest circle & retrieve additional output data
    for (unsigned i = 0; i < n_patches; i++) {
        col_max[i] *= resolution_xy;
        col_min[i] *= resolution_xy;
        row_max[i] *= resolution_xy;
        row_min[i] *= resolution_xy;

        circles[i] = makeSmallestEnclosingCircle(border_points[i]);
        circle_area[i] = M_PI * pow(circles[i].r, 2);
        circle_center_x[i] = circles[i].c.x;
        circle_center_y[i] = nrows  * resolution_xy - circles[i].c.y; // to flip the coordinate system in the row direction
        circle_diameter[i] = circles[i].r * 2;

        patch_width[i] = ((col_max[i] + resolution_xy) - (col_min[i]));
        patch_height[i] = ((row_max[i] + resolution_xy) - (row_min[i]));
        patch_area[i] *= resolution_xy * resolution_xy;
    }

    DataFrame retval = DataFrame::create(Named("patch_id") = IntegerVector(patch_id.begin(), patch_id.end()),
                                         Named("patch_area") = IntegerVector(patch_area.begin(), patch_area.end()),
                                         Named("patch_height") = IntegerVector(patch_height.begin(), patch_height.end()),
                                         Named("patch_width") = IntegerVector(patch_width.begin(), patch_width.end()),
                                         Named("circle_center_x") = NumericVector(circle_center_x.begin(), circle_center_x.end()),
                                         Named("circle_center_y") = NumericVector(circle_center_y.begin(), circle_center_y.end()),
                                         Named("circle_diameter") = NumericVector(circle_diameter.begin(), circle_diameter.end()),
                                         Named("circle_area") = NumericVector(circle_area.begin(), circle_area.end()));

    return retval;
}
