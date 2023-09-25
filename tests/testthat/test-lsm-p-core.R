landscapemetrics_patch_landscape_value <- lsm_p_core(landscape)

test_that("lsm_p_core works for a single class landscape", {
    landscape_single_patch <- landscape
    landscape_single_patch[] <- 1
    result <- lsm_p_core(landscape_single_patch, consider_boundary = TRUE)
    expect_equal(result$value, 0.09)
})

test_that("lsm_p_core is typestable", {
    expect_s3_class(lsm_p_core(landscape), "tbl_df")
    expect_s3_class(lsm_p_core(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_core(landscape_list), "tbl_df")
})

test_that("lsm_p_core returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_core returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_core can set edge depth typestable", {
    expect_s3_class(lsm_p_core(landscape, edge_depth = 3), "tbl_df")
})

test_that("lsm_p_core equals FRAGSTATS", {
    lsm_landscape <- lsm_p_core(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_core(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "core") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "core") |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape), tolerance = 0.01)
    expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta), tolerance = 0.01)
})
