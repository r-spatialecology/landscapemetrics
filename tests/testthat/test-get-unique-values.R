context("get_unique_values")

vector_x <- as.integer(c(1, 1, 1, 2, 1, 2, 2))
list_x <- list(landscape, landscape_matrix, vector_x)
list_y <- list(list_x, landscape_stack)

test_that("get_unique_values works for vector", {

    expect_is(get_unique_values(vector_x), class = "list")
    expect_length(get_unique_values(vector_x), n = 1)

    expect_is(get_unique_values(vector_x, simplify = TRUE),
              class = "integer")
    expect_equal(get_unique_values(vector_x, simplify = TRUE),
                 expected = c(1, 2))

    expect_warning(get_unique_values(as.numeric(vector_x)),
                   regexp = "Double values will be converted to integer.",
                   fixed = TRUE)
})

test_that("get_unique_values works for matrix", {

    expect_is(get_unique_values(landscape_matrix), class = "list")
    expect_length(get_unique_values(landscape_matrix), n = 1)

    expect_is(get_unique_values(landscape_matrix, simplify = TRUE),
              class = "integer")
    expect_equal(get_unique_values(landscape_matrix, simplify = TRUE),
                 expected = c(1, 2, 3))
})

test_that("get_unique_values works for list", {

    expect_is(get_unique_values(list_x), class = "list")
    expect_length(get_unique_values(list_x), n = 3)

    expect_warning(get_unique_values(list_x, simplify = TRUE),
                   regexp = "Not able to simply list with more than 1 element.",
                   fixed = TRUE)
})

test_that("get_unique_values works for RasterLayers", {

    expect_is(get_unique_values(landscape), class = "list")
    expect_length(get_unique_values(landscape), n = 1)

    expect_is(get_unique_values(landscape, simplify = TRUE), class = "integer")
    expect_equal(get_unique_values(landscape, simplify = TRUE),
                 expected = c(1, 2, 3))
})

test_that("get_unique_values works for RasterStack", {

    expect_is(get_unique_values(landscape_stack), class = "list")
    expect_length(get_unique_values(landscape_stack), n = 2)

    expect_warning(get_unique_values(landscape_stack, simplify = TRUE),
                   regexp = "Not able to simplify RasterStack.",
                   fixed = TRUE)
})

test_that("get_unique_values works for RasterBrick", {

    expect_is(get_unique_values(landscape_brick), class = "list")
    expect_length(get_unique_values(landscape_brick), n = 2)

    expect_warning(get_unique_values(landscape_brick, simplify = TRUE),
                   regexp = "Not able to simplify RasterBrick",
                   fixed = TRUE)
})

test_that("get_unique_values works only for correct data types", {

expect_error(get_unique_values(list_y),
             regexp = "List elements must be a RasterLayer, matrix or vector.",
             fixed = TRUE)
})


test_that("get_unique_values works for RasterLayers not in memory", {

    landscape2 <- raster::writeRaster(landscape, tempfile())
    expect_is(get_unique_values(landscape2), class = "list")
    expect_length(get_unique_values(landscape2), n = 1)

    expect_is(get_unique_values(landscape2, simplify = TRUE), class = "integer")
    expect_equal(get_unique_values(landscape2, simplify = TRUE),
                 expected = c(1, 2, 3))
})

