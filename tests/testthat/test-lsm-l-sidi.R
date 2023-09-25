landscapemetrics_landscape_landscape_value <- lsm_l_sidi(landscape)

test_that("lsm_l_sidi is typestable", {
    expect_s3_class(lsm_l_sidi(landscape), "tbl_df")
    expect_s3_class(lsm_l_sidi(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_sidi(landscape_list), "tbl_df")
})

test_that("lsm_l_sidi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_sidi returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_sidi equals FRAGSTATS", {
    lsm_landscape <- lsm_l_sidi(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_sidi(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "sidi") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "sidi") |> dplyr::pull(value)

    expect_equal(object = lsm_landscape, expected = fs_landcape, tolerance = 0.01)
    expect_equal(object = lsm_augusta, expected = fs_augusta, tolerance = 0.01)
})
