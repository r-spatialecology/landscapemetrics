context("window_lsm")

window <- matrix(1, nrow = 3, ncol = 3)

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

    expect_is(window_lsm(landscape, window = window, what = "lsm_l_pr"),
              class = "list")

    expect_is(window_lsm(landscape_stack, window = window, what = "lsm_l_pr"),
              class = "list")

    expect_is(window_lsm(landscape_brick, window = window, what = "lsm_l_pr"),
              class = "list")

    expect_is(window_lsm(landscape_list, window = window, what = "lsm_l_pr"),
              class = "list")
})

test_that("window_lsm returns all errors", {

    expect_error(window_lsm(landscape, window = window, what = "lsm_p_area"),
                 grep = "'window_lsm()' is only able to calculate landscape level metrics.",
                 fixed = TRUE)
})

