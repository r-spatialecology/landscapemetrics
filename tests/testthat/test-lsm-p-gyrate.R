landscapemetrics_patch_landscape_value <- lsm_p_gyrate(landscape)

test_that("lsm_p_gyrate is typestable", {
    expect_s3_class(lsm_p_gyrate(landscape), "tbl_df")
    expect_s3_class(lsm_p_gyrate(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_gyrate(landscape_list), "tbl_df")
})

test_that("lsm_p_gyrate returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_gyrate can force centroid to be within patch", {

    result_a <- lsm_p_gyrate(landscape)
    result_b <- lsm_p_gyrate(landscape, cell_center = TRUE)

    expect_true(object = any(result_a$value != result_b$value))
})

test_that("lsm_p_gyrate returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

# https://fragstats.org/index.php/fragstats-metrics/area-and-edge-metrics/p3-radius-of-gyration
# GYRATE = 0 when the patch consists of a single cell and increases without limit as the patch increases in extent

# test_that("lsm_p_frac equals FRAGSTATS", {
#     lsm_landscape <- lsm_p_gyrate(landscape) |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
#     lsm_augusta <- lsm_p_gyrate(augusta_nlcd) |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
#
#     fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "gyrate") |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
#     fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "gyrate") |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
#
#     expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape), tolerance = 0.01)
#     expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta), tolerance = 0.01)
# })
