context("get_adjacencies")


adjacencies_full <- get_adjacencies(landscape, 4, what = "full")
adjacencies_like <- get_adjacencies(landscape, 4, what = "like")
adjacencies_unlike <- get_adjacencies(landscape, 4, what = "unlike")
adjacencies_triangle <- get_adjacencies(landscape, 4, what = "triangle")

test_that("get_adjacencies runs and returns a matrix", {
    expect_is(adjacencies_full, "matrix")
    expect_is(adjacencies_like, "matrix")
    expect_is(adjacencies_unlike, "matrix")
    expect_is(adjacencies_triangle, "matrix")

    expect_true(is.na(adjacencies_triangle[1,3]))
})

adjacencies_upperfull <- get_adjacencies(landscape, 4, what = "full", upper = TRUE)
adjacencies_upperlike <- get_adjacencies(landscape, 4, what = "like", upper = TRUE)
adjacencies_upperunlike <- get_adjacencies(landscape, 4, what = "unlike", upper = TRUE)
adjacencies_uppertriangle <- get_adjacencies(landscape, 4, what = "triangle", upper = TRUE)

test_that("get_adjacencies runs also for the upper triangle", {
    expect_is(adjacencies_upperfull, "matrix")
    expect_is(adjacencies_upperlike, "matrix")
    expect_is(adjacencies_upperunlike, "matrix")
    expect_is(adjacencies_uppertriangle, "matrix")

    expect_true(is.na(adjacencies_uppertriangle[3,1]))
})

new_r <- raster::raster(nrows = 3, ncols = 3, vals = c(rep(1, 8), 2))
adjacencies_4 <- get_adjacencies(new_r, 4)
adjacencies_8 <- get_adjacencies(new_r, 8)
diagonal_matrix <- matrix(c(1,  NA,  1,
                            NA,  0, NA,
                            1,  NA,  1), 3, 3, byrow = TRUE)
adjacencies_diag <- get_adjacencies(new_r, diagonal_matrix)

test_that("get_adjacencies works for different values of neighborhood than 4", {
    expect_equivalent(adjacencies_4, matrix(c(20L, 2L, 2L, 0L), ncol = 2))
    expect_equivalent(adjacencies_8, matrix(c(34L, 3L, 3L, 0L), ncol = 2))
    expect_equivalent(adjacencies_diag, matrix(c(14L, 1L, 1L, 0L), ncol = 2))

})
