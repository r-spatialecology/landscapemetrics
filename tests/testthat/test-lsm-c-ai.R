landscapemetrics_class_landscape_value <- lsm_c_ai(landscape)

test_that("lsm_c_ai is typestable", {
    expect_s3_class(lsm_c_ai(landscape), "tbl_df")
    expect_s3_class(lsm_c_ai(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_ai(landscape_list), "tbl_df")
})

test_that("lsm_c_ai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_ai handles NAs correctly", {
    r <- terra::rast(nrow = 10, ncol = 10)
    r[] <- 1
    expect_equal(lsm_c_ai(r)$value, 100)
    r[1, 1] <- NA
    expect_equal(lsm_c_ai(r)$value, 100)
    r[2, 5] <- NA
    expect_equal(lsm_c_ai(r)$value, 98.86364, tolerance = 0.00001)
})

test_that("lsm_c_ai returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_ai equals FRAGSTATS", {
    lsm_landscape <- lsm_c_ai(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_c_ai(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_class, LID == "landscape", metric == "ai") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_class, LID == "augusta_nlcd", metric == "ai") |> dplyr::pull(value)

    expect_true(test_relative(obs = lsm_landscape, exp = fs_landscape, tolerance = tol_rel))
    expect_true(test_relative(obs = lsm_augusta, exp = fs_augusta, tolerance = tol_rel))
})
