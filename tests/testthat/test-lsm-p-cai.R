landscapemetrics_patch_landscape_value <- lsm_p_cai(landscape)

test_that("lsm_p_cai is typestable", {
    expect_s3_class(lsm_p_cai(landscape), "tbl_df")
    expect_s3_class(lsm_p_cai(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_cai(landscape_list), "tbl_df")
})

test_that("lsm_p_cai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_cai returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_cai equals FRAGSTATS", {
    lsm_landscape <- lsm_p_cai(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_cai(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "cai") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "cai") |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape), tolerance = 0.01)
    expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta), tolerance = 0.01)
})
