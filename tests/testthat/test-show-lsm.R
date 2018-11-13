context("test-show-lsm")

test_that("show_lsm returns a plot", {
    patches_area <- show_lsm(landscape, what = "lsm_p_area")
    expect_is(patches_area, "ggplot")
})

test_that("show_lsm returns error if metric not available returns a plot", {
    expect_error(show_lsm(landscape, what = "lsm_l_ta"))
    expect_error(show_lsm(landscape, what = "lsm_p_area", class = 5))
})
