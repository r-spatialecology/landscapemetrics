context("get_adjacencies")

test_that("get_adjacencies runs for all data types", {

    raster_layer <- get_adjacencies(landscape)
    raster_stack <- get_adjacencies(landscape_stack)
    raster_list <- get_adjacencies(landscape_list)
    raster_matrix <- get_adjacencies(landscape_matrix)

    expect_is(raster_layer, "list")
    expect_is(raster_stack, "list")
    expect_is(raster_list, "list")
    expect_is(raster_matrix, "list")
})

test_that("get_adjacencies runs and returns a matrix", {

    adjacencies_full <- get_adjacencies(landscape,
                                        neighbourhood = 4, what = "full")

    adjacencies_like <- get_adjacencies(landscape,
                                        neighbourhood = 4, what = "like")

    adjacencies_unlike <- get_adjacencies(landscape,
                                          neighbourhood = 4, what = "unlike")

    adjacencies_triangle <- get_adjacencies(landscape,
                                            neighbourhood = 4, what = "triangle")

    expect_true(all(!is.na(adjacencies_full[[1]])))
    expect_true(all(is.na(adjacencies_like[[1]][-c(1, 5, 9)])))
    expect_true(all(is.na(adjacencies_unlike[[1]][-c(2, 3, 6)])))
    expect_true(all(is.na(adjacencies_triangle[[1]][-c(1, 2, 3, 5, 6, 9)])))
})

test_that("get_adjacencies runs also for the upper triangle", {

    adjacencies_upperfull <- get_adjacencies(landscape,
                                             neighbourhood = 4, what = "full", upper = TRUE)

    adjacencies_upperlike <- get_adjacencies(landscape,
                                             neighbourhood = 4, what = "like", upper = TRUE)

    adjacencies_upperunlike <- get_adjacencies(landscape,
                                               neighbourhood = 4, what = "unlike", upper = TRUE)

    adjacencies_uppertriangle <- get_adjacencies(landscape,
                                                 neighbourhood = 4, what = "triangle", upper = TRUE)

    expect_true(all(!is.na(adjacencies_upperfull[[1]])))
    expect_true(all(is.na(adjacencies_upperlike[[1]][-c(1, 5, 9)])))
    expect_true(all(is.na(adjacencies_upperunlike[[1]][-c(4, 7,8)])))
    expect_true(all(is.na(adjacencies_uppertriangle[[1]][-c(1, 4, 5, 7, 8, 9)])))
})

test_that("get_adjacencies works for different values of neighborhood than 4", {

    new_ras <- terra::rast(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))

    adjacencies_4 <- get_adjacencies(new_ras, neighbourhood = 4)
    adjacencies_8 <- get_adjacencies(new_ras, neighbourhood = 8)

    diagonal_matrix <- matrix(c(1,  NA,  1,
                                NA,  0, NA,
                                1,  NA,  1), 3, 3, byrow = TRUE)

    adjacencies_diag <- get_adjacencies(new_ras, diagonal_matrix)

    expect_equivalent(adjacencies_4[[1]], matrix(c(20L, 2L, 2L, 0L), ncol = 2))
    expect_equivalent(adjacencies_8[[1]], matrix(c(34L, 3L, 3L, 0L), ncol = 2))
    expect_equivalent(adjacencies_diag[[1]], matrix(c(14L, 1L, 1L, 0L), ncol = 2))

})


test_that("get_adjacencies returns error", {

    expect_error(get_adjacencies(landscape, neighbourhood = 12),
                 regexp = "neighbourhood must be either 4, 8 or a binary matrix where the ones define the neighbourhood.")
})

