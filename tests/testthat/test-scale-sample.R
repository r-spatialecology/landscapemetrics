test_that("scale_sample works for a matrix", {

    result_mat <- scale_sample(landscape, y = sample_points, size = c(5, 10, 15),
                               shape = "circle", what = c("lsm_l_ta", "lsm_l_np"),
                               verbose = FALSE)

    expect_s3_class(object = result_mat, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_mat$metric))
    expect_true(all(c(5, 10, 15) %in% result_mat$size))

})

test_that("scale_sample works for a sf", {

    result_mat <- scale_sample(landscape, y = points_sf, size = c(5, 10, 15),
                               shape = "circle", what = c("lsm_l_ta", "lsm_l_np"),
                               verbose = FALSE)

    expect_s3_class(object = result_mat, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_mat$metric))
    expect_true(all(c(5, 10, 15) %in% result_mat$size))

})

test_that("scale_sample forwards arguments to calculate_lsm", {

    result_mat <- scale_sample(landscape, y = points_sf, size = c(5, 10, 15),
                               shape = "circle", what = "lsm_p_core", edge_depth = 100,
                               verbose = FALSE)

    expect_true(all(result_mat$value == 0))

})
