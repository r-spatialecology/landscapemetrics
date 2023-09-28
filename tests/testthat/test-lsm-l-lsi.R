landscapemetrics_landscape_landscape_value <- lsm_l_lsi(landscape)

test_that("lsm_l_lsi is typestable", {
    expect_s3_class(lsm_l_lsi(landscape), "tbl_df")
    expect_s3_class(lsm_l_lsi(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_lsi(landscape_list), "tbl_df")
})

test_that("lsm_l_lsi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_lsi returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_lsi equals FRAGSTATS", {
    lsm_landscape <- lsm_l_lsi(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_lsi(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "lsi") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "lsi") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
