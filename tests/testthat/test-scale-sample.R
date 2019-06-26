context("scale_sample")

# use matrix
sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

# use sp points
sample_points_sp <- sp::SpatialPoints(sample_points)

test_that("scale_sample works for a matrix", {

    result_mat <- scale_sample(landscape,
                               y = sample_points,
                               size = 5, max_size = 15,
                               verbose = FALSE,
                               what = c("lsm_l_ta", "lsm_l_np"))

    expect_is(object = result_mat, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_mat$metric))
    expect_equal(object = unique(result_mat$size), expected = c(5, 10, 15))
})

test_that("scale_sample works for a sp points", {

    result_sp <- scale_sample(landscape,
                              y = sample_points_sp,
                              size = 5, max_size = 15,
                              verbose = FALSE,
                              what = c("lsm_l_ta", "lsm_l_np"))

    expect_is(object = result_sp, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_sp$metric))
    expect_equal(object = unique(result_sp$size), expected = c(5, 10, 15))
})

test_that("scale_sample forwards arguments to calculate_lsm", {

    result_mat <- scale_sample(landscape,
                               y = sample_points,
                               size = 5, max_size = 15,
                               what = "lsm_p_core",
                               edge_depth = 100,
                               verbose = FALSE)

    expect_true(all(result_mat$value == 0))
})

test_that("scale_sample works for all data type", {

    result_stack <- scale_sample(landscape_stack,
                                 y = sample_points,
                                 size = 5, max_size = 15,
                                 what = "lsm_l_ta",
                                 verbose = FALSE)

    result_brick <- scale_sample(landscape_brick,
                                 y = sample_points,
                                 size = 5, max_size = 15,
                                 what = "lsm_l_ta",
                                 verbose = FALSE)

    result_list <- scale_sample(landscape_list,
                                y = sample_points,
                                size = 5, max_size = 15,
                                what = "lsm_l_ta",
                                verbose = FALSE)

    expect_is(result_stack, class = "tbl_df")
    expect_is(result_brick, class = "tbl_df")
    expect_is(result_list, class = "tbl_df")

    expect_equal(object = nrow(result_stack), expected = 18)
    expect_equal(object = nrow(result_brick), expected = 18)
    expect_equal(object = nrow(result_list), expected = 18)

    expect_true("ta" %in% result_stack$metric)
    expect_true("ta" %in% result_brick$metric)
    expect_true("ta" %in% result_list$metric)
})

test_that("scale_sample returns warnings", {

    expect_warning(scale_sample(landscape,
                                y = sample_points,
                                size = 20, max_size = 20,
                                what = "lsm_l_ta"),
                   regexp = "Some of buffers extend over the landscape border. Consider decreasing of the max_size argument value.",
                   fixed = TRUE)

})

test_that("scale_sample returns errors", {

    expect_error(scale_sample(landscape,
                              y = 1:3,
                              size = 20, max_size = 20,
                              what = "lsm_l_ta"),
                   regexp = "'y' must be a matrix or SpatialPoints.",
                   fixed = TRUE)

})
