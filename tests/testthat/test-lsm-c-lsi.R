landscapemetrics_class_landscape_value <- lsm_c_lsi(landscape)

test_that("lsm_c_lsi is typestable", {
    expect_s3_class(lsm_c_lsi(landscape), "tbl_df")
    expect_s3_class(lsm_c_lsi(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_lsi(landscape_list), "tbl_df")
})

test_that("lsm_c_lsi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_lsi returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_lsi equals FRAGSTATS", {
    lsm_landscape <- lsm_c_lsi(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_c_lsi(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_class, LID == "landscape", metric == "lsi") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_class, LID == "augusta_nlcd", metric == "lsi") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
