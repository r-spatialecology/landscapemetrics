context("show_cores")

cores_landscape <- show_cores(landscape)

cores_landscape_stack <- show_cores(landscape_stack, class = 1)

cores_landscape_brick <- show_cores(landscape_brick, class = "all")

cores_landscape_list <- show_cores(landscape_list, class = c(2,3))


test_that("show_cores returns a plot", {

    expect_equal(class(cores_landscape), c("gg","ggplot"))
})

test_that("show_cores returns a plot for class = global", {

    expect_equal(class(show_cores(landscape, class = "global")), c("gg","ggplot"))
})

test_that("show_patches returns a plot for each list entry", {

    expect_true(all(vapply(cores_landscape_stack,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
    expect_true(all(vapply(cores_landscape_brick,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
    expect_true(all(vapply(cores_landscape_list,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
})

test_that("show_patches returns error if class is not present", {

    expect_error(show_cores(landscape, class = 42),
                 regexp = "class must at least contain one value of a class contained in the landscape.",
                 fixed = TRUE)
})

test_that("show_cores can increase edge_depth", {

    expect_equal(class(show_cores(landscape, edge_depth = 3, class = "global")), c("gg","ggplot"))
})

test_that("show_cores returns warnning for wrong combination of class argument", {

    expect_warning(show_cores(landscape, class = c(1, "global")),
                   regexp = "'global' and 'all' can't be combined with any other class-argument.",
                   fixed = TRUE)
})

