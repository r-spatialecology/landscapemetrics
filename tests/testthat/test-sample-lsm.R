context("sample_lsm")

points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
sample_matrix_test <- sample_lsm(landscape, points = points, size = 15, what = "patch")
sample_matrix_test_circle <- sample_lsm(landscape, points = points, size = 15, shape = 'circle', what = "patch")

sample_matrix_test_stack <- sample_lsm(landscape_stack, points = points, size = 15, what = "patch")
sample_matrix_test_stack_circle <- sample_lsm(landscape_stack, points = points, size = 15, shape = 'circle', what = "patch")

sample_matrix_test_brick <- sample_lsm(landscape_brick, points = points, size = 15, what = "patch")
sample_matrix_test_brick_circle <- sample_lsm(landscape_brick, points = points, size = 15, shape = 'circle', what = "patch")

sample_matrix_test_list <- sample_lsm(landscape_list, points = points, size = 15, what = "patch")
sample_matrix_test_list_circle <- sample_lsm(landscape_list, points = points, size = 15, shape = 'circle', what = "patch")

points_sp <- sp::SpatialPoints(points)
sample_points_test <- sample_lsm(landscape, points = points_sp, size = 15, what = "patch")
sample_points_test_circle <- sample_lsm(landscape, points = points_sp, size = 15, shape = 'circle', what = "patch")

sample_points_test_stack <- sample_lsm(landscape_stack, points = points_sp, size = 15, what = "patch")
sample_points_test_stack_circle <- sample_lsm(landscape_stack, points = points_sp, size = 15, shape = 'circle', what = "patch")

sample_points_test_brick <- sample_lsm(landscape_brick, points = points_sp, size = 15, what = "patch")
sample_points_test_brick_circle <- sample_lsm(landscape_brick, points = points_sp, size = 15, shape = 'circle', what = "patch")

sample_points_test_list <- sample_lsm(landscape_list, points = points_sp, size = 15, what = "patch")
sample_points_test_list_circle <- sample_lsm(landscape_list, points = points_sp, size = 15, shape = 'circle', what = "patch")


test_that("extract_lsm returns a tbl", {
    expect_is(sample_matrix_test, "tbl_df")
    expect_is(sample_points_test, "tbl_df")
    expect_is(sample_matrix_test_circle, "tbl_df")
    expect_is(sample_points_test_circle, "tbl_df")

    expect_is(sample_matrix_test_stack, "tbl_df")
    expect_is(sample_points_test_stack, "tbl_df")
    expect_is(sample_matrix_test_stack_circle, "tbl_df")
    expect_is(sample_points_test_stack_circle, "tbl_df")

    expect_is(sample_matrix_test_brick, "tbl_df")
    expect_is(sample_points_test_brick, "tbl_df")
    expect_is(sample_matrix_test_brick_circle, "tbl_df")
    expect_is(sample_points_test_brick_circle, "tbl_df")

    expect_is(sample_matrix_test_list, "tbl_df")
    expect_is(sample_points_test_list, "tbl_df")
    expect_is(sample_matrix_test_list_circle, "tbl_df")
    expect_is(sample_points_test_list_circle, "tbl_df")
})
