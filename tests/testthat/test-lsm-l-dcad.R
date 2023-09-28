landscapemetrics_landscape_landscape_value <- lsm_l_dcad(landscape)

test_that("lsm_l_dcad is typestable", {
    expect_s3_class(lsm_l_dcad(landscape), "tbl_df")
    expect_s3_class(lsm_l_dcad(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_dcad(landscape_list), "tbl_df")
})

test_that("lsm_l_dcad returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_dcad returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_dcad equals FRAGSTATS", {
    lsm_landscape <- lsm_l_dcad(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_dcad(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "dcad") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "dcad") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
