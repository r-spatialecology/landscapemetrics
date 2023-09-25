landscapemetrics_patch_landscape_value <- lsm_p_enn(landscape)

test_that("lsm_p_enn is typestable", {
    expect_s3_class(lsm_p_enn(landscape), "tbl_df")
    expect_s3_class(lsm_p_enn(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_enn(landscape_list), "tbl_df")
})

test_that("lsm_p_enn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_enn returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_area equals FRAGSTATS", {
    lsm_landscape <- lsm_p_enn(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_enn(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "enn") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "enn") |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape), tolerance = 0.01)
    expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta), tolerance = 0.01)
})
