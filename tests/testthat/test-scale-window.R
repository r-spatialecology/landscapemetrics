context("scale_window")

percentages_col <- c(50, 100)
percentages_row <- c(50, 100)

test_that("scale_window returns data frame with percentages", {

    skip_on_cran()
    skip_on_ci()

    result <- scale_window(landscape = landscape,
                           percentages_col = percentages_col,
                           percentages_row = percentages_row,
                           what = "lsm_l_ta", stat = "mean",
                           verbose = FALSE)

    expect_is(object = result, class = "tbl_df")
    expect_equal(object = nrow(result), expected = 2)
    expect_equal(object = result$percentages_col, expected = c(50, 100))
    expect_equal(object = result$percentages_row, expected = c(50, 100))
})

test_that("scale_window forwards arguments", {

    skip_on_cran()
    skip_on_ci()

    result <- scale_window(landscape = landscape,
                           percentages_col = percentages_col,
                           percentages_row = percentages_row,
                           what = "lsm_l_core_mn", edge_depth = 10,
                           stat = "mean",
                           verbose = FALSE)

    expect_equal(object = result$value, expected = c(0, 0))
})

test_that("scale_window works for all data types", {

    skip_on_cran()
    skip_on_ci()

    result_stack <- scale_window(landscape = landscape_stack,
                                 percentages_col = percentages_col,
                                 percentages_row = percentages_row,
                                 what = "lsm_l_ta", stat = "mean",
                                 verbose = FALSE)

    result_brick <- scale_window(landscape = landscape_brick,
                                 percentages_col = percentages_col,
                                 percentages_row = percentages_row,
                                 what = "lsm_l_ta", stat = "mean",
                                 verbose = FALSE)

    result_list <- scale_window(landscape = landscape_list,
                                percentages_col = percentages_col,
                                percentages_row = percentages_row,
                                what = "lsm_l_ta", stat = "mean",
                                verbose = FALSE)

    expect_is(object = result_stack, class = "tbl_df")
    expect_is(object = result_brick, class = "tbl_df")
    expect_is(object = result_list, class = "tbl_df")

    expect_equal(object = nrow(result_stack), expected = 4)
    expect_equal(object = nrow(result_brick), expected = 4)
    expect_equal(object = nrow(result_list), expected = 4)
})

test_that("scale_window returns warnings", {

    skip_on_cran()
    skip_on_ci()

    expect_error(scale_window(landscape = landscape,
                              percentages_col = percentages_col,
                              percentages_row = percentages_row,
                              what = "lsm_p_area", stat = "mean",
                              verbose = FALSE),
                   regexp = "'window_lsm()' is only able to calculate landscape level metrics.",
                   fixed = TRUE)
})
