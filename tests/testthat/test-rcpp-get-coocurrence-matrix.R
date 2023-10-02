foo <- function(x, directions = "rook") {

    adjacencies <- terra::adjacent(x = x, cells = 1:terra::ncell(x),
                                   directions = directions, pairs = TRUE)

    r <- table(terra::values(x, mat = FALSE)[adjacencies[,1]],
          terra::values(x, mat = FALSE)[adjacencies[,2]])

    matrix(r, nrow = nrow(r), ncol = ncol(r))
}

landscape_na <- terra::rast(matrix(c(1, 1, NA, 2, 2, 1), ncol = 2))

land_true1 <- foo(landscape)
land_true2 <- foo(podlasie_ccilc)
land_true3 <- foo(landscape_na)
land_true4 <- foo(landscape, "queen")

land_result1 <- rcpp_get_coocurrence_matrix(terra::as.matrix(landscape, wide = TRUE), directions = as.matrix(4))
dimnames(land_result1) <- NULL

land_result2 <- rcpp_get_coocurrence_matrix(terra::as.matrix(podlasie_ccilc, wide = TRUE), directions = as.matrix(4))
dimnames(land_result2) <- NULL

land_result3 <- rcpp_get_coocurrence_matrix(terra::as.matrix(landscape_na, wide = TRUE), directions = as.matrix(4))
dimnames(land_result3) <- NULL

land_result4 <- rcpp_get_coocurrence_matrix(terra::as.matrix(landscape, wide = TRUE), directions = as.matrix(8))
dimnames(land_result4) <- NULL

test_that("rcpp_get_coocurrence_matrix results are correct", {
    expect_equal(land_result1, land_true1)
    expect_equal(land_result2, land_true2)
    expect_equal(land_result3, land_true3)
    expect_equal(land_result4, land_true4)
})

test_that("rcpp_get_coocurrence_matrix is typestable", {

    expect_true(is.matrix(land_result1))

})

land_true1_v <- as.vector(land_true1)
land_true4_v <- as.vector(land_true4)

land_result1_v <- rcpp_get_coocurrence_vector(terra::as.matrix(landscape, wide = TRUE), directions = as.matrix(4))
land_result4_v <- rcpp_get_coocurrence_vector(terra::as.matrix(landscape, wide = TRUE), directions = as.matrix(8))

test_that("rcpp_get_coocurrence_vector results are correct", {
    expect_equal(land_result1_v, land_true1_v)
    expect_equal(land_result4_v, land_true4_v)
})

test_that("rcpp_get_coocurrence_vector is typestable", {
    expect_type(land_result1_v, "double")
})

land_true1_cv <- c(table(terra::values(landscape, mat = FALSE)))

land_result1_cv <- rcpp_get_composition_vector(terra::as.matrix(landscape, wide = TRUE))

test_that("rcpp_get_composition_vector results are correct", {
    expect_equal(land_result1_cv, land_true1_cv)
})

test_that("rcpp_get_composition_vector is typestable", {
    expect_type(land_result1_cv, "integer")
})

test_that("triangular_index works properly", {
    expect_equal(triangular_index(0, 0), 0)
    expect_equal(triangular_index(2, 1), 4)
    expect_type(triangular_index(0, 1), "integer")
})
