context("show_patches")

patches_landscape <- show_patches(landscape)

patches_landscape_stack <- show_patches(landscape_stack, class = 1)

patches_landscape_brick <- show_patches(landscape_brick, class = "all")

patches_landscape_list <- show_patches(landscape_list, class = c(2,3))


test_that("show_patches returns a plot", {
    expect_equal(class(patches_landscape), c("gg","ggplot"))
})

test_that("show_patches returns a plot for each list entry", {
    expect_true(all(vapply(patches_landscape_stack,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
    expect_true(all(vapply(patches_landscape_brick,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
    expect_true(all(vapply(patches_landscape_list,
                           FUN = function(x) class(x)[2],
                           FUN.VALUE = character(1)) == "ggplot"))
})

test_that("show_patches returns warnings and errors", {

    expect_warning(show_patches(landscape, class = c(1, "global")),
                   regexp = "'global' and 'all' can't be combined with any other class-argument.")

    expect_error(show_patches(landscape, class = 42),
                 regexp = "'class' must at least contain one value of a class contained in the landscape.")
})
