context("show_cores")

cores_landscape <- show_cores(landscape)

cores_landscape_stack <- show_cores(landscape_stack, what = 1)

cores_landscape_brick <- show_cores(landscape_brick, what = "all")

cores_landscape_list <- show_cores(landscape_list, what = c(2,3))


test_that("show_cores returns a plot", {
    expect_equal(class(cores_landscape), c("gg","ggplot"))
})

test_that("show_cores returns a plot for what = global", {
    expect_equal(class(show_cores(landscape, what = "global")), c("gg","ggplot"))
})

test_that("show_patches returns a plot for each list entry", {
    expect_true(all(sapply(cores_landscape_stack, FUN = function(x) class(x)[2]) == "ggplot"))
    expect_true(all(sapply(cores_landscape_brick, FUN = function(x) class(x)[2]) == "ggplot"))
    expect_true(all(sapply(cores_landscape_list, FUN = function(x) class(x)[2]) == "ggplot"))
})

test_that("show_patches returns error if class is not present", {
    expect_error(show_cores(landscape, what = 42),
                 regexp = "what must at least contain one value of a class contained in the landscape.")
})

test_that("show_cores can increase edge_depth", {
    expect_equal(class(show_cores(landscape, edge_depth = 3, what = "global")), c("gg","ggplot"))
})

