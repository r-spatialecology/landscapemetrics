context("show_cores")

cores_landscape <- show_cores(landscape)

cores_landscape_stack <- show_cores(landscape_stack, class = 1)

cores_landscape_list <- show_cores(landscape_list, class = c(2,3))

test_that("show_cores returns a plot for each list entry", {


    expect_true(all(vapply(cores_landscape,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))

    expect_true(all(vapply(cores_landscape_stack,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))

    expect_true(all(vapply(cores_landscape_list,  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))
})

test_that("show_cores returns a plot for class = global", {

    expect_true(all(vapply(show_cores(landscape, class = "global"),  FUN = function(x)
        inherits(x = x,  c("gg","ggplot")), FUN.VALUE = logical(1))))

})

test_that("show_patches returns error if class is not present", {

    expect_error(show_cores(landscape, class = 42),
                 regexp = "class must at least contain one value of a class contained in the landscape.",
                 fixed = TRUE)
})

test_that("show_cores can increase edge_depth", {

    expect_true(all(vapply(show_cores(landscape, edge_depth = 3, class = "global"),
                           FUN = function(x) inherits(x = x,  c("gg","ggplot")),
                           FUN.VALUE = logical(1))))

})

test_that("show_cores returns warnning for wrong combination of class argument", {

    expect_warning(show_cores(landscape, class = c(1, "global")),
                   regexp = "'global' and 'all' can't be combined with any other class-argument.",
                   fixed = TRUE)
})

