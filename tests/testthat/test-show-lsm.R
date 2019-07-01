context("test-show-lsm")

test_that("show_lsm returns a plot", {

    patches_area <- show_lsm(landscape, what = "lsm_p_area")

    expect_is(patches_area, "ggplot")
})

test_that("show_lsm returns a facet plot", {

    patches_area <- show_lsm(landscape, what = "lsm_p_area",
                             class = c(1, 3),
                             labels = FALSE)

    expect_is(patches_area$facet, "FacetWrap")
})

test_that("show_lsm can handle stacks, bricks and lists", {

    plots_list <- show_lsm(landscape_list, what = "lsm_p_area")
    plots_stack <- show_lsm(landscape_stack, what = "lsm_p_area")
    plots_brick <- show_lsm(landscape_brick, what = "lsm_p_area")

    expect_is(plots_list[[1]], "ggplot")
    expect_is(plots_list[[2]], "ggplot")

    expect_is(plots_stack[[1]], "ggplot")
    expect_is(plots_stack[[2]], "ggplot")

    expect_is(plots_brick[[1]], "ggplot")
    expect_is(plots_brick[[2]], "ggplot")
})

test_that("show_lsm returns warnings and errors", {

    expect_warning(show_lsm(landscape, what = "lsm_p_area", class = c(1, "global")),
                   regexp = "'global' and 'all' can't be combined with any other class-argument.",
                   fixed = TRUE)

    expect_error(show_lsm(landscape, what = "lsm_p_invented_metric"),
                 regexp = "Please provide one patch level metric only. To list available metrics, run list_lsm(level = 'patch').",
                 fixed = TRUE)

    expect_error(show_lsm(landscape, what = "lsm_p_area", class = 5),
                 regexp = "'class' must contain at least one value of a class existing in the landscape.",
                 fixed = TRUE)
})
