landscapemetrics_landscape_landscape_value <- lsm_l_pladj(landscape)

test_that("lsm_l_pladj is typestable", {
    expect_s3_class(lsm_l_pladj(landscape), "tbl_df")
    expect_s3_class(lsm_l_pladj(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_pladj(landscape_list), "tbl_df")
})

test_that("lsm_l_pladj returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_pladj returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_pladj equals FRAGSTATS", {
    lsm_landscape <- lsm_l_pladj(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_pladj(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "pladj") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "pladj") |> dplyr::pull(value)

    expect_true(test_relative(obs = lsm_landscape, exp = fs_landscape, tolerance = tol_rel))
    expect_true(test_relative(obs = lsm_augusta, exp = fs_augusta, tolerance = tol_rel))
})

