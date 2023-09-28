landscapemetrics_patch_landscape_value <- lsm_p_shape(landscape)

test_that("lsm_p_shape is typestable", {
    expect_s3_class(lsm_p_shape(landscape), "tbl_df")
    expect_s3_class(lsm_p_shape(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_shape(landscape_list), "tbl_df")
})

test_that("lsm_p_shape returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_shape returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

# MH: Something wrong

test_that("lsm_p_shape equals FRAGSTATS", {
    # lsm_landscape <- lsm_p_shape(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_shape(augusta_nlcd) |> dplyr::pull(value)

    # fs_landscape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "shape") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "shape") |> dplyr::pull(value)

    # expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
