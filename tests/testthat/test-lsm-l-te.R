landscapemetrics_landscape_landscape_value <- lsm_l_te(landscape)

test_lsm <- matrix(data = NA, nrow = 25, ncol = 30)

test_lsm[c(5:7), c(5:7)] <- 1
test_lsm[4, 6] <- 1
test_lsm[6, 8] <- 1
test_lsm[8, 6] <- 1
test_lsm[6, 4] <- 1
test_lsm[6, 6] <- 2

test_lsm <- terra::rast(test_lsm)

test_that("lsm_l_te is typestable", {
    expect_s3_class(lsm_l_te(landscape), "tbl_df")
    expect_s3_class(lsm_l_te(landscape_stack), "tbl_df")
    expect_s3_class(lsm_l_te(landscape_list), "tbl_df")
})

test_that("lsm_l_te returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_te returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_te option count_boundary is working", {
    te_with_boundary <- lsm_l_te(landscape, count_boundary = TRUE)
    te_without_boundary <- lsm_l_te(landscape, count_boundary = FALSE)
    expect_lt(te_without_boundary$value, te_with_boundary$value)
})

test_that("lsm_l_te can handle raster with different xy resolution", {
    expect_s3_class(lsm_l_te(landscape_diff_res), "tbl_df")
})


test_that("lsm_l_te is the same if count_boundary = FALSE", {

    result_cbF <- lsm_c_te(test_lsm, count_boundary = FALSE)
    result_cbT <- lsm_c_te(test_lsm, count_boundary = TRUE)

    result_l_cbF <- lsm_l_te(test_lsm, count_boundary = FALSE)
    result_l_cbT <- lsm_l_te(test_lsm, count_boundary = TRUE)

    expect_true(all(result_l_cbF$value == result_cbF$value))
    expect_true(all(result_l_cbT$value == max(result_cbT$value)))
})

test_that("lsm_l_te equals FRAGSTATS", {
    lsm_landscape <- lsm_l_te(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_l_te(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_landscape, LID == "landscape", metric == "te") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_landscape, LID == "augusta_nlcd", metric == "te") |> dplyr::pull(value)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})
