landscapemetrics_landscape_landscape_value <- lsm_l_ai(landscape)

test_that("lsm_l_ai is typestable", {
    expect_s3_class(lsm_l_ai(landscape), "tbl_df")
    expect_s3_class(lsm_l_ai(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_ai(landscape_list), "tbl_df")
})

test_that("lsm_l_ai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_ai returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_ai equals FRAGSTATS", {
    lsm_landscape <- lsm_l_ai(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_ai(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "ai") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "ai") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
