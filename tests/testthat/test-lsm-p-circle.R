context("patch level lsm_p_circle metric")

landscapemetrics_patch_landscape_value <- lsm_p_circle(landscape)

test_that("lsm_p_circle is typestable", {
    expect_is(lsm_p_circle(landscape), "tbl_df")
    expect_is(lsm_p_circle(landscape_stack), "tbl_df")
    expect_is(lsm_p_circle(landscape_list), "tbl_df")
})

test_that("lsm_p_circle returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_circle returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_circle can also handle irregular sized cells", {
    expect_error(object = lsm_p_circle(landscape_diff_res),
                 regexp = "The area of the circumscribing circle is currently only implemented for equal resolutions.")
})
