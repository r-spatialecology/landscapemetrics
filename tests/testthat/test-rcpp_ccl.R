context("rcpp_ccl")

mat8 <- matrix(c(NA, 1L, NA, NA, 1L, 1L, 1L, NA), 4, 2)
mat4 <- matrix(c(NA, 1L, NA, NA, 1L, 1L, 1L, NA), 4, 2)

landscapemetrics:::rcpp_ccl(mat4, 4)
landscapemetrics:::rcpp_ccl(mat8, 8)

test_that("rcpp_ccl patch labelling is correct", {
    expect_equal(length(get_unique_values(mat4, simplify = TRUE)), 1)
    expect_equal(length(get_unique_values(mat8, simplify = TRUE)), 1)
})
