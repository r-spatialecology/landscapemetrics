context("get_circumscribingcircle")

class_1 <- get_patches(landscape, class = 1)[[1]]

class_1_irr <- get_patches(landscape_diff_res, class = 1)[[1]]

class_1_mat <- raster_to_points(class_1, return_NA = FALSE)

test_that("get_circumscribingcircle has one radius for each patch", {

    circle <- get_circumscribingcircle(class_1)

    expect_equal(nrow(circle),
                     expected =  max(get_unique_values(class_1)[[1]]))
})

test_that("get_circumscribingcircle has two rows and is tibble", {

    circle <- get_circumscribingcircle(class_1)

    expect_is(circle, class = "tbl")
    expect_equal(ncol(circle), expected = 2)

})

test_that("get_circumscribingcircle works for irregular raster", {

    circle <- get_circumscribingcircle(class_1_irr)

    expect_is(circle, class = "tbl")
    expect_equal(ncol(circle), expected = 2)
    expect_equal(nrow(circle),
                 expected =  max(get_unique_values(class_1_irr)[[1]]))

})

test_that("get_circumscribingcircle returns errors", {

    expect_error(object = get_circumscribingcircle(class_1_mat),
                 grep = "Resolution must be provided to correctly calculate the edges.")

    expect_error(object = get_circumscribingcircle(class_1_mat[, 1:2]),
                 grep = "Coordinate matrix must have 3 (x, y, id) columns.")

})

test_that("get_circumscribingcircle works for coordinate matrix", {

    circle <- get_circumscribingcircle(class_1_mat,
                                       resolution_x = 1,
                                       resolution_y = 1)

    expect_equal(nrow(circle),
                 expected =  max(class_1_mat[, 3]))

    expect_is(circle, class = "tbl")
    expect_equal(ncol(circle), expected = 2)
})

