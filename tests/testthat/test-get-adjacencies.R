context("get_adjacencies")


adjacencies_full <- get_adjacencies(landscape, 4, what = "full")
adjacencies_like <- get_adjacencies(landscape, 4, what = "like")
adjacencies_unlike <- get_adjacencies(landscape, 4, what = "unlike")
adjacencies_triangle <- get_adjacencies(landscape, 4, what = "triangle")

test_that("get_adjacencies runs and returns a matrix", {
    expect_is(adjacencies_full[[1]], "matrix")
    expect_is(adjacencies_like[[1]], "matrix")
    expect_is(adjacencies_unlike[[1]], "matrix")
    expect_is(adjacencies_triangle[[1]], "matrix")

    expect_true(is.na(adjacencies_triangle[[1]][1,3]))
})

adjacencies_upperfull <- get_adjacencies(landscape, 4, what = "full", upper = TRUE)
adjacencies_upperlike <- get_adjacencies(landscape, 4, what = "like", upper = TRUE)
adjacencies_upperunlike <- get_adjacencies(landscape, 4, what = "unlike", upper = TRUE)
adjacencies_uppertriangle <- get_adjacencies(landscape, 4, what = "triangle", upper = TRUE)

test_that("get_adjacencies runs also for the upper triangle", {
    expect_is(adjacencies_upperfull[[1]], "matrix")
    expect_is(adjacencies_upperlike[[1]], "matrix")
    expect_is(adjacencies_upperunlike[[1]], "matrix")
    expect_is(adjacencies_uppertriangle[[1]], "matrix")

    expect_true(is.na(adjacencies_uppertriangle[[1]][3,1]))
})

new_r <- raster::raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
adjacencies_4 <- get_adjacencies(new_r, 4)
adjacencies_8 <- get_adjacencies(new_r, 8)
diagonal_matrix <- matrix(c(1,  NA,  1,
                            NA,  0, NA,
                            1,  NA,  1), 3, 3, byrow = TRUE)
adjacencies_diag <- get_adjacencies(new_r, diagonal_matrix)

test_that("get_adjacencies works for different values of neighborhood than 4", {
    expect_equivalent(adjacencies_4[[1]], matrix(c(20L, 2L, 2L, 0L), ncol = 2))
    expect_equivalent(adjacencies_8[[1]], matrix(c(34L, 3L, 3L, 0L), ncol = 2))
    expect_equivalent(adjacencies_diag[[1]], matrix(c(14L, 1L, 1L, 0L), ncol = 2))

})

test_that("get_adjacencies works for different data types", {

    result_stack <- get_adjacencies(landscape_stack)
    result_brick <- get_adjacencies(landscape_brick)
    result_list <- get_adjacencies(landscape_list)
    result_matrix <- get_adjacencies(landscape_matrix)

    expect_is(object = result_stack, "list")
    expect_is(object = result_brick, "list")
    expect_is(object = result_list, "list")
    expect_is(object = result_matrix, "list")
})
