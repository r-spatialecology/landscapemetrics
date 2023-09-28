landscapemetrics_landscape_landscape_value <- lsm_l_pafrac(landscape)

test_that("lsm_l_pafrac is typestable", {

    expect_s3_class(lsm_l_pafrac(landscape), "tbl_df")
    expect_s3_class(lsm_l_pafrac(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_pafrac(landscape_list), "tbl_df")
})

test_that("lsm_l_pafrac returns the desired number of columns", {

    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_pafrac returns in every column the correct type", {

    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_pafrac throws error for less than 10 patches",  {

    expect_warning(lsm_l_pafrac(landscape_uniform),
                   regexp = "PAFRAC = NA for NP < 10",
                   fixed = TRUE)
})

test_that("lsm_l_pafrac equals FRAGSTATS", {
    lsm_landscape <- lsm_l_pafrac(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_pafrac(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "pafrac") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "pafrac") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
