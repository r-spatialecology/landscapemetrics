context("scale_window")

percentages_col <- c(50, 100)
percentages_row <- c(50, 100)

test_that("scale_window works returns data frame with percentages ", {

    result <- scale_window(landscape = landscape,
                           percentages_col = percentages_col,
                           percentages_row = percentages_row,
                           what = "lsm_l_ta", stat = "mean")

    expect_is(object = result, class = "tbl_df")
    expect_equal(object = nrow(result), expected = 2)
    expect_equal(object = result$percentages_col, expected = c(50, 100))
    expect_equal(object = result$percentages_row, expected = c(50, 100))
})

test_that("scale_window takes argument", {

    result <- scale_window(landscape = landscape,
                           percentages_col = percentages_col,
                           percentages_row = percentages_row,
                           what = "lsm_l_core_mn", edge_depth = 10,
                           stat = "mean")

    expect_equal(object = result$value, expected = c(0, 0))
})

test_that("window_lsm returns works for all data types", {


    result_stack <- scale_window(landscape = landscape_stack,
                                 percentages_col = percentages_col,
                                 percentages_row = percentages_row,
                                 what = "lsm_l_ta", stat = "mean")

    result_brick <- scale_window(landscape = landscape_brick,
                                 percentages_col = percentages_col,
                                 percentages_row = percentages_row,
                                 what = "lsm_l_ta", stat = "mean")

    result_list <- scale_window(landscape = landscape_list,
                                percentages_col = percentages_col,
                                percentages_row = percentages_row,
                                what = "lsm_l_ta", stat = "mean")

    expect_is(object = result_stack, class = "tbl_df")
    expect_is(object = result_brick, class = "tbl_df")
    expect_is(object = result_list, class = "tbl_df")

    expect_equal(object = nrow(result_stack), expected = 4)
    expect_equal(object = nrow(result_brick), expected = 4)
    expect_equal(object = nrow(result_list), expected = 4)
})

test_that("scale_window returns warnings", {

    expect_error(scale_window(landscape = landscape,
                                percentages_col = percentages_col,
                                percentages_row = percentages_row,
                                what = "lsm_p_area", stat = "mean"),
                   regexp = "'window_lsm()' is only able to calculate landscape level metrics.",
                   fixed = TRUE)
})
