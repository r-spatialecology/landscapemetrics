context("show_patches")

patches_landscape <- show_patches(landscape)


patches_landscape_stack <- show_patches(landscape_stack, class = 1)

patches_landscape_list <- show_patches(landscape_list, class = c(2,3))

test_that("show_patches returns a plot for each list entry", {

    expect_true(all(vapply(patches_landscape,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))

    expect_true(all(vapply(patches_landscape_stack,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))

    expect_true(all(vapply(patches_landscape_list,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))
})

test_that("show_patches returns warnings and errors", {

    expect_warning(show_patches(landscape, class = c(1, "global")),
                   regexp = "'global' and 'all' can't be combined with any other class-argument.",
                   fixed = TRUE)

    expect_error(show_patches(landscape, class = 42),
                 regexp = "'class' must at least contain one value of a class contained in the landscape.",
                 fixed = TRUE)
})
