context("class level cai_mn metric")


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
