landscapemetrics_class_landscape_value <- lsm_c_ca(landscape)

test_that("lsm_c_ca is typestable", {
    expect_s3_class(lsm_c_ca(landscape), "tbl_df")
    expect_s3_class(lsm_c_ca(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_ca(landscape_list), "tbl_df")
})

test_that("lsm_c_ca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_ca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_ca equals FRAGSTATS", {
    lsm_landscape <- lsm_c_ca(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_c_ca(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_class, LID == "landscape", metric == "ca") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_class, LID == "augusta_nlcd", metric == "ca") |> dplyr::pull(value)

    expect_true(test_relative(obs = lsm_landscape, exp = fs_landscape, tolerance = tol_rel))
    expect_true(test_relative(obs = lsm_augusta, exp = fs_augusta, tolerance = tol_rel))
})
