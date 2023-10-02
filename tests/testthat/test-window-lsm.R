window <- matrix(1, nrow = 11, ncol = 11)

window_even <-  matrix(1, nrow = 10, ncol = 10)

test_that("window_lsm returns a list with selected metrics", {

    result <- window_lsm(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))

    expect_equal(names(result[[1]]), expected = c("lsm_l_joinent", "lsm_l_pr"))

    expect_length(result[[1]], n = 2)
})

test_that("window_lsm takes argument", {

    result <- window_lsm(landscape, window = window, what = "lsm_l_core_mn", edge_depth = 10)

    expect_true(all(result[[1]][[1]][] == 0))
})

test_that("window_lsm returns works for all data types", {

    expect_type(window_lsm(landscape, window = window, what = "lsm_l_pr"),
                type = "list")

    expect_type(window_lsm(landscape_stack, window = window, what = "lsm_l_pr"),
                type = "list")

    expect_type(window_lsm(landscape_list, window = window, what = "lsm_l_pr"),
                type = "list")
})

test_that("window_lsm returns all errors", {

    expect_error(window_lsm(landscape, window = window, what = "lsm_p_area"),
                 regexp = "'window_lsm()' is only able to calculate landscape level metrics.",
                 fixed = TRUE)

    expect_error(window_lsm(landscape, window = window_even, what = "lsm_l_pr"),
                 regexp = "The window must have uneven sides.",
                 fixed = TRUE)
})

