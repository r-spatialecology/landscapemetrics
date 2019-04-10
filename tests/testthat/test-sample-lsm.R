context("sample_lsm")

# use a matrix
sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

# use sp points
points_sp <- sp::SpatialPoints(sample_points)

# use polygons
poly_1 <-  sp::Polygon(cbind(c(2.5, 2.5, 17.5, 17.5),
                           c(-2.5, 12.5, 12.5, -2.5)))
poly_2 <-  sp::Polygon(cbind(c(7.5, 7.5, 23.5, 23.5),
                           c(-7.5, 23.5, 23.5, -7.5)))
poly_1 <- sp::Polygons(list(poly_1), "p1")
poly_2 <- sp::Polygons(list(poly_2), "p2")

sample_plots <- sp::SpatialPolygons(list(poly_1, poly_2))

# use lines
x1 <- c(1, 5, 15, 10)
y1 <- c(1, 5, 15, 25)

x2 <- c(10, 25)
y2 <- c(5, 5)

sample_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(x1, y1)), sp::Line(cbind(x2, y2))), ID = "a")))

test_that("sample_lsm works for a matrix", {

    result_mat <- sample_lsm(landscape,
                             sample_points = sample_points, size = 15,
                             shape = "circle",
                             what = c("lsm_l_ta", "lsm_l_np"))

    expect_is(object = result_mat, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_mat$metric))
})

test_that("sample_lsm works for sp points", {

    result_sp <- sample_lsm(landscape,
                            sample_points = points_sp, size = 15,
                            shape = "square",
                            what = "lsm_l_np")

    expect_is(object = result_sp, class = "tbl_df")

    expect_true(all(c("np") %in% result_sp$metric))
})

test_that("sample_lsm works for polygons ", {

    result_poly <- sample_lsm(landscape,
                              sample_plots = sample_plots,
                              level = "patch")

    expect_is(object = result_poly, class = "tbl_df")

    expect_true(all("patch" %in% result_poly$level))

    if (!nzchar(system.file(package = "rgeos"))) {
        expect_warning(sample_lsm(landscape,
                                  sample_plots = sample_plots,
                                  what = "lsm_p_area"),
                       regexp = "Package 'rgeos' not installed. Please make sure polygons are disaggregated.")
    }
})

test_that("sample_lsm works for lines ", {

    if (!nzchar(system.file(package = "rgeos"))) {

        expect_error(sample_lsm(landscape,
                                sample_lines = sample_lines,
                                level = "landscape"),
                     regexp = "To sample landscape metrics in buffers around lines, the package 'rgeos' must be installed.")

    }

    else {

        result_lines <- sample_lsm(landscape,
                                   sample_lines = sample_lines,
                                   level = "landscape")

        expect_is(object = result_lines, class = "tbl_df")

        expect_true(all("landscape" %in% result_lines$level))
    }
})

test_that("sample_lsm works forwards arguments to calculate_lsm", {

    result_mat <- sample_lsm(landscape,
                             sample_points = sample_points, size = 15,
                             shape = "circle",
                             what = "lsm_p_core",
                             edge_depth = 100)

    expect_true(all(result_mat$value == 0))
})

test_that("sample_lsm works for all data type", {

    result_stack <- sample_lsm(landscape_stack,
                               sample_plots = sample_plots,
                               what = "lsm_l_ta")

    result_brick <- sample_lsm(landscape_brick,
                              sample_plots = sample_plots,
                              what = "lsm_l_ta")

    result_list <- sample_lsm(landscape_list,
                              sample_plots = sample_plots,
                              what = "lsm_l_ta")

    expect_is(result_stack, class = "tbl_df")
    expect_is(result_brick, class = "tbl_df")
    expect_is(result_list, class = "tbl_df")

    expect_equal(object = result_stack$layer,
                 expected = c(1, 1, 2, 2))
    expect_equal(object = result_brick$layer,
                 expected = c(1, 1, 2, 2))
    expect_equal(object = result_list$layer,
                 expected = c(1, 1, 2, 2))

    expect_true("ta" %in% result_stack$metric)
    expect_true("ta" %in% result_brick$metric)
    expect_true("ta" %in% result_list$metric)

})

test_that("sample_lsm returns errors", {

    expect_error(sample_lsm(landscape,
                            sample_points = sample_points, size = 15,
                            shape = "rectangle",
                            what = c("lsm_l_ta", "lsm_l_np")),
                 regexp = "Shape = rectangle unknown.")

    expect_error(sample_lsm(landscape),
                 regexp = "Please provide either sample point locations, sample lines or sample plots.")

    expect_error(sample_lsm(landscape,
                            sample_points = sample_points,
                            sample_lines = sample_lines,
                            sample_plots = sample_plots,
                            size = 10),
                 regexp = "Please provide only sample point locations, sample lines or only sample plots.")

    expect_error(sample_lsm(landscape,
                            sample_points = sample_points,
                            sample_plots = sample_plots,
                            size = 10),
                 regexp = "Please provide only sample point locations, sample lines or only sample plots.")

    expect_error(sample_lsm(landscape,
                            sample_plots = 1:3),
                 regexp = "Sample plots must be SpatialPolygons.")

    expect_error(sample_lsm(landscape,
                            sample_points = 1:3, size = 15,
                            shape = "rectangle",
                            what = c("lsm_l_ta", "lsm_l_np")),
                 regexp = "Sample points must be a matrix or SpatialPoints.")
})
