landscapemetrics_landscape_landscape_value <- lsm_l_shei(landscape)

test_that("lsm_l_shei is typestable", {
    expect_s3_class(lsm_l_shei(landscape), "tbl_df")
    expect_s3_class(lsm_l_shei(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_shei(landscape_list), "tbl_df")
})

test_that("lsm_l_shei returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_shei returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_shei returns 0 when only one patch present", {
    landscape_simple <- landscape
    landscape_simple[] = 1
    expect_equal(lsm_l_shei(landscape_simple)$value, 0)
})

test_that("lsm_l_shei equals FRAGSTATS", {
    lsm_landscape <- lsm_l_shei(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_shei(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "shei") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "shei") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
