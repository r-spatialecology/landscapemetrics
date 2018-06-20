context("landscape level mesh metric")

fragstats_landscape_landscape_mesh <- fragstats_landscape_landscape$MESH
landscapemetrics_landscape_landscape_mesh <- lsm_l_mesh(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_mesh %in%
                        round(landscapemetrics_landscape_landscape_mesh$value,4)))
})

test_that("lsm_c_mesh is typestable", {
    expect_is(landscapemetrics_landscape_landscape_mesh, "tbl_df")
    expect_is(lsm_l_mesh(landscape_stack), "tbl_df")
    expect_is(lsm_l_mesh(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_mesh), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_mesh$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_mesh$level, "character")
    expect_type(landscapemetrics_landscape_landscape_mesh$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_mesh$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_mesh$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_mesh$value, "double")
})


