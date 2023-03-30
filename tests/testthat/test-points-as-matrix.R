test_that("points_as_mat returns matrix for all data types", {

    expect_true(is.matrix(points_as_mat(sample_points)))
    # expect_true(is.matrix(points_as_mat(points_sp)))
    # expect_true(is.matrix(points_as_mat(points_spdf)))
    # expect_true(is.matrix(points_as_mat(points_multi)))
    # expect_true(is.matrix(points_as_mat(points_sf)))
    # expect_true(is.matrix(points_as_mat(points_sfc)))
    expect_true(is.matrix(points_as_mat(points_terra)))

})
