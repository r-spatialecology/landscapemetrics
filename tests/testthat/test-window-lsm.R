context("window_lsm")

window <- matrix(1, nrow = 3, ncol = 3)

test_that("window_lsm returns a list with selected metrics", {

    result <- window_lsm(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))

    expect_equal(names(result[[1]]), expected = c("lsm_l_joinent", "lsm_l_pr"))
    expect_length(result[[1]], n = 2)
})

test_that("window_lsm returns workds for all data types", {

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

    result <- window_lsm(landscape, window = window, what = c("lsm_l_pr", "lsm_l_joinent"))

    expect_error(window_lsm(landscape, window = window, level = "patch"),
                 regexp = "window_lsm is only able to calculate landscape level metrics.")
})

