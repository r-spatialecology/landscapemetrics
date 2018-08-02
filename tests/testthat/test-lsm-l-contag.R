context("landscape level contag metric")

landscapemetrics_patch_landscape_contag <- lsm_l_contag(landscape)

test_that("lsm_l_contag is typestable", {
    expect_is(landscapemetrics_patch_landscape_contag, "tbl_df")
    expect_is(lsm_l_contag(landscape_stack), "tbl_df")
    expect_is(lsm_l_contag(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_contag returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_contag), 6)
})

test_that("lsm_p_contag returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_contag$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_contag$level, "character")
    expect_type(landscapemetrics_patch_landscape_contag$class, "integer")
    expect_type(landscapemetrics_patch_landscape_contag$id, "integer")
    expect_type(landscapemetrics_patch_landscape_contag$metric, "character")
    expect_type(landscapemetrics_patch_landscape_contag$value, "double")
})

test_that("lsm_p_contag returns a warning for < 3 classes", {
    rast_mat <- matrix(c(NA,1,NA,1,1,1,NA,1,NA), ncol = 3, byrow = TRUE)
    rast_rast <- raster::raster(rast_mat)
    expect_warning(lsm_l_contag(rast_rast))
})
