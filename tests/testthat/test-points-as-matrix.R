context("points_as_mat")

test_that("points_as_mat returns matrix for all data types", {

    expect_is(points_as_mat(sample_points), "matrix")
    expect_is(points_as_mat(points_sp), "matrix")
    expect_is(points_as_mat(points_spdf), "matrix")
    expect_is(points_as_mat(points_multi), "matrix")
    expect_is(points_as_mat(points_sf), "matrix")
    expect_is(points_as_mat(points_sfc), "matrix")
    expect_is(points_as_mat(points_terra), "matrix")

})
