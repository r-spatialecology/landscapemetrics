landscapemetrics_landscape_landscape_value <- lsm_l_msidi(landscape)

test_that("lsm_l_msidi is typestable", {
    expect_s3_class(lsm_l_msidi(landscape), "tbl_df")
    expect_s3_class(lsm_l_msidi(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_msidi(landscape_list), "tbl_df")
})

test_that("lsm_l_msidi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_msidi returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_msidi equals FRAGSTATS", {
    lsm_landscape <- lsm_l_msidi(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_msidi(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "msidi") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "msidi") |> dplyr::pull(value)

    expect_true(test_relative(obs = lsm_landscape, exp = fs_landscape, tolerance = tol_rel))
    expect_true(test_relative(obs = lsm_augusta, exp = fs_augusta, tolerance = tol_rel))
})


