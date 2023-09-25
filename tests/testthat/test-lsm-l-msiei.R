landscapemetrics_landscape_landscape_value <- lsm_l_msiei(landscape)

test_that("lsm_l_msiei is typestable", {
    expect_s3_class(lsm_l_msiei(landscape), "tbl_df")
    expect_s3_class(lsm_l_msiei(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_msiei(landscape_list), "tbl_df")
})

test_that("lsm_l_msiei returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_msiei returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_msiei equals FRAGSTATS", {
    lsm_landscape <- lsm_l_msiei(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_msiei(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "msiei") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "msiei") |> dplyr::pull(value)

    expect_equal(object = lsm_landscape, expected = fs_landcape, tolerance = 0.01)
    expect_equal(object = lsm_augusta, expected = fs_augusta, tolerance = 0.01)
})

