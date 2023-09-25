landscapemetrics_landscape_landscape_value <- lsm_l_ta(landscape)

test_that("lsm_l_ta is typestable", {
    expect_s3_class(lsm_l_ta(landscape), "tbl_df")
    expect_s3_class(lsm_l_ta(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_ta(landscape_list), "tbl_df")
})

test_that("lsm_l_ta returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_ta returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_ta equals FRAGSTATS", {
    lsm_landscape <- lsm_l_ta(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_ta(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "ta") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "ta") |> dplyr::pull(value)

    expect_equal(object = lsm_landscape, expected = fs_landcape)
    expect_equal(object = lsm_augusta, expected = fs_augusta)
})
