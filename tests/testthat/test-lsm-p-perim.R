context("patch level lsm_p_perim metric")

landscapemetrics_patch_landscape_value <- lsm_p_perim(landscape)

test_that("lsm_p_perim is typestable", {
    expect_is(lsm_p_perim(landscape), "tbl_df")
    expect_is(lsm_p_perim(landscape_stack), "tbl_df")
    expect_is(lsm_p_perim(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_perim returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_perim returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_perim can also handle irregular sized cells", {
    landscape_irr <- raster::raster(resolution = c(50, 100))
    landscape_irr[] <- sample(1:14, 14, replace = TRUE)
    landscapemetrics_patch_landscape_value <- lsm_p_perim(landscape_irr)
    expect_is(landscapemetrics_patch_landscape_value, "tbl_df")
})
