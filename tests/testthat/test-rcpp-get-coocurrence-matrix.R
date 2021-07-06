context("coocurrence matrix")

landscape_na <- matrix(c(1, 1, NA, 2, 2, 1), ncol = 2)

land_true1 <- structure(c(520L, 43L, 137L, 43L, 704L, 184L, 137L, 184L, 1528L),
                       .Dim = c(3L, 3L),
                       .Dimnames = structure(list(c("1", "2", "3"), c("1", "2", "3")),
                                             .Names = c("", "")))

land_true2 <- structure(c(121354L, 33908L, 15997L, 126L, 930L, 2L, 3732L, 182L,
                           1597L, 23L, 13347L, 632L, 580L, 318L, 33908L, 74946L, 6664L,
                           67L, 213L, 0L, 1099L, 61L, 366L, 0L, 2784L, 34L, 1515L, 171L,
                           15997L, 6664L, 26500L, 142L, 1218L, 2L, 4272L, 304L, 2292L, 29L,
                           7040L, 117L, 241L, 117L, 126L, 67L, 142L, 290L, 23L, 0L, 319L,
                           15L, 85L, 3L, 155L, 4L, 20L, 1L, 930L, 213L, 1218L, 23L, 19768L,
                           135L, 584L, 1866L, 1272L, 53L, 1254L, 1203L, 25L, 15L, 2L, 0L,
                           2L, 0L, 135L, 100L, 4L, 26L, 34L, 0L, 0L, 29L, 0L, 0L, 3732L,
                           1099L, 4272L, 319L, 584L, 4L, 74496L, 5008L, 3100L, 27L, 719L,
                           77L, 246L, 404L, 182L, 61L, 304L, 15L, 1866L, 26L, 5008L, 16906L,
                           1023L, 18L, 76L, 32L, 10L, 53L, 1597L, 366L, 2292L, 85L, 1272L,
                           34L, 3100L, 1023L, 5910L, 22L, 791L, 111L, 34L, 45L, 23L, 0L,
                           29L, 3L, 53L, 0L, 27L, 18L, 22L, 110L, 88L, 3L, 0L, 0L, 13347L,
                           2784L, 7040L, 155L, 1254L, 0L, 719L, 76L, 791L, 88L, 64594L,
                           1467L, 68L, 37L, 632L, 34L, 117L, 4L, 1203L, 29L, 77L, 32L, 111L,
                           3L, 1467L, 21428L, 4L, 91L, 580L, 1515L, 241L, 20L, 25L, 0L,
                           246L, 10L, 34L, 0L, 68L, 4L, 5072L, 31L, 318L, 171L, 117L, 1L,
                           15L, 0L, 404L, 53L, 45L, 0L, 37L, 91L, 31L, 3394L),
                       .Dim = c(14L, 14L),
                       .Dimnames = structure(list(c("10", "11", "30", "40", "60",
                                                    "61", "70", "90", "100", "110",
                                                    "130", "180", "190", "210"),
                                                  c("10", "11", "30", "40", "60",
                                                    "61", "70", "90", "100", "110",
                                                    "130", "180", "190", "210")),
                                             .Names = c("", "")))

land_true3 <- structure(c(2L, 3L, 3L, 2L),
                       .Dim = c(2L, 2L),
                       .Dimnames = structure(list(c("1", "2"), c("1", "2")),
                                             .Names = c("", "")))

land_true4 <- structure(c(986L, 96L, 304L, 96L, 1306L, 424L, 304L, 424L, 2904L),
                       .Dim = c(3L, 3L),
                       .Dimnames = structure(list(c("1", "2", "3"),
                                                  c("1", "2", "3")),
                                             .Names = c("", "")))

land_result1 <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape), directions = as.matrix(4))
land_result2 <- rcpp_get_coocurrence_matrix(raster::as.matrix(podlasie_ccilc), directions = as.matrix(4))
land_result3 <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape_na), directions = as.matrix(4))
land_result4 <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape), directions = as.matrix(8))

test_that("rcpp_get_coocurrence_matrix results are correct", {
    expect_equivalent(land_result1, land_true1)
    expect_equivalent(land_result2, land_true2)
    expect_equivalent(land_result3, land_true3)
    expect_equivalent(land_result4, land_true4)
})

test_that("rcpp_get_coocurrence_matrix is typestable", {
    expect_is(land_result1, "matrix")
})

context("coocurrence vector")

land_true1_v <- as.vector(land_true1)
land_true4_v <- as.vector(land_true4)

land_result1_v <- rcpp_get_coocurrence_vector(raster::as.matrix(landscape), directions = as.matrix(4))
land_result4_v <- rcpp_get_coocurrence_vector(raster::as.matrix(landscape), directions = as.matrix(8))

test_that("rcpp_get_coocurrence_vector results are correct", {
    expect_equivalent(land_result1_v, land_true1_v)
    expect_equivalent(land_result4_v, land_true4_v)
})

test_that("rcpp_get_coocurrence_vector is typestable", {
    expect_is(land_result1_v, "numeric")
})

context("composition vector")

land_true1_cv <- unclass(table(landscape@data@values))

land_result1_cv <- rcpp_get_composition_vector(raster::as.matrix(landscape))

test_that("rcpp_get_composition_vector results are correct", {
    expect_equivalent(land_result1_cv, land_true1_cv)
})

test_that("rcpp_get_composition_vector is typestable", {
    expect_is(land_result1_cv, "integer")
})

test_that("triangular_index works properly", {
    expect_equal(triangular_index(0, 0), 0)
    expect_equal(triangular_index(2, 1), 4)
    expect_type(triangular_index(0, 1), "integer")
})

