landscapemetrics_class_landscape_value <- lsm_c_iji(landscape)

test_that("lsm_c_iji is typestable", {

    expect_s3_class(lsm_c_iji(landscape), "tbl_df")
    expect_s3_class(lsm_c_iji(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_iji(landscape_list), "tbl_df")
})

test_that("lsm_c_iji returns the desired number of columns", {

    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_iji returns in every column the correct type", {

    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_iji returns warning for less than 3 classes", {

    expect_warning(lsm_c_iji(landscape_simple),
                   regexp = "Number of classes must be >= 3, IJI = NA.",
                   fixed = TRUE)
})

test_that("lsm_c_iji equals FRAGSTATS", {
    lsm_landscape <- lsm_c_iji(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_c_iji(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_class, LID == "landscape", metric == "iji") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_class, LID == "augusta_nlcd", metric == "iji") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})

