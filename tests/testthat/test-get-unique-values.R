vector_x <- as.integer(c(1, 1, 1, 2, 1, 2, 2))
list_x <- list(landscape, landscape_matrix, vector_x)
list_y <- list(list_x, landscape_stack)

test_that("get_unique_values works for vector", {

    expect_type(get_unique_values(vector_x), type = "list")

    expect_length(get_unique_values(vector_x), n = 1)

    expect_type(get_unique_values(vector_x, simplify = TRUE),
                type = "integer")

    expect_equal(get_unique_values(vector_x, simplify = TRUE),
                 expected = c(1, 2))

    expect_warning(get_unique_values(as.numeric(vector_x)),
                   regexp = "Double values will be converted to integer.",
                   fixed = TRUE)
})

test_that("get_unique_values works for matrix", {

    expect_type(get_unique_values(landscape_matrix), type = "list")

    expect_length(get_unique_values(landscape_matrix), n = 1)

    expect_type(get_unique_values(landscape_matrix, simplify = TRUE), type = "integer")

    expect_equal(get_unique_values(landscape_matrix, simplify = TRUE),
                 expected = c(1, 2, 3))
})

test_that("get_unique_values works for list", {

    expect_type(get_unique_values(list_x), type = "list")
    expect_length(get_unique_values(list_x), n = 3)

    expect_warning(get_unique_values(list_x, simplify = TRUE),
                   regexp = "Not able to simplify input with more than one layer.",
                   fixed = TRUE)
})

test_that("get_unique_values works for RasterLayers", {

    expect_type(get_unique_values(landscape), type = "list")
    expect_length(get_unique_values(landscape), n = 1)

    expect_type(get_unique_values(landscape, simplify = TRUE), type = "integer")
    expect_equal(get_unique_values(landscape, simplify = TRUE),
                 expected = c(1, 2, 3))
})

test_that("get_unique_values works for RasterStack", {

    expect_type(get_unique_values(landscape_stack), type = "list")
    expect_length(get_unique_values(landscape_stack), n = 2)

    expect_warning(get_unique_values(landscape_stack, simplify = TRUE),
                   regexp = "Not able to simplify input with more than one layer.",
                   fixed = TRUE)
})

test_that("get_unique_values works only for correct data types", {

expect_error(get_unique_values(list_y),
             regexp = "Input must be vector, matrix, raster, stars, or terra object or list of previous.",
             fixed = TRUE)
})
