landscapemetrics_landscape_landscape_value <- lsm_l_prd(landscape)

test_that("lsm_l_prd is typestable", {
    expect_s3_class(lsm_l_prd(landscape), "tbl_df")
    expect_s3_class(lsm_l_prd(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_prd(landscape_list), "tbl_df")
})

test_that("lsm_l_prd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_prd returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_prd equals FRAGSTATS", {
    lsm_landscape <- lsm_l_prd(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_prd(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "prd") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "prd") |> dplyr::pull(value)

    expect_equal(object = lsm_landscape, expected = fs_landcape)
    expect_equal(object = lsm_augusta, expected = fs_augusta, tolerance = 0.01)
})
