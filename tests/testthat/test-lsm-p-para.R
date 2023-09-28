landscapemetrics_patch_landscape_value <- lsm_p_para(landscape)

test_that("lsm_p_para is typestable", {
    expect_s3_class(lsm_p_para(landscape), "tbl_df")
    expect_s3_class(lsm_p_para(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_para(landscape_list), "tbl_df")
})

test_that("lsm_p_para returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_para returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

# https://fragstats.org/index.php/fragstats-metrics/shape-metrics/p1-perimeter-area-ratio
# PARA equals the ratio of the patch perimeter (m) to area (m2).

test_that("lsm_p_para equals FRAGSTATS", {
    lsm_landscape <- lsm_p_para(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_para(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "para") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "para") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape / 10000, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta / 10000, tol = tolerance))
})
