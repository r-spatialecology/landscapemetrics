# T-shaped patch with 7 cells horizontal and 6 cells vertical
mat1 <- matrix(data = c(NA,5,NA,NA,NA,NA,NA,
                        NA,5,NA,NA,NA,NA,NA,
                        NA,5,NA,NA,NA,NA,NA,
                        NA,5, 5, 5, 5, 5, 5,
                        NA,5,NA,NA,NA,NA,NA,
                        NA,5,NA,NA,NA,NA,NA,
                        NA,5,NA,NA,NA,NA,NA), nrow = 7, ncol = 7)
mat1_cir <- rcpp_get_circle(mat = mat1)
patch_id <- 5
patch_height <- 6
patch_width <- 7
circle_x <- 3.5
circle_y <- 4
circle_diameter <- sqrt(64 + 1)
circle_area <- (circle_diameter/2)^2 * pi

test_that("rcpp_get_circle patch calculates T-shaped patches correctly", {
    expect_equal(mat1_cir$patch_id, patch_id)
    expect_equal(mat1_cir$patch_height, patch_height)
    expect_equal(mat1_cir$patch_width, patch_width)
    expect_equal(mat1_cir$circle_center_x, circle_x)
    expect_equal(mat1_cir$circle_center_y, circle_y)
    expect_equal(mat1_cir$circle_diameter, circle_diameter)
    expect_equal(mat1_cir$circle_area, circle_area)
})

mat1_cir2 <- rcpp_get_circle(mat = mat1, 2) # double resolution

test_that("rcpp_get_circle handles double resolution", {
    expect_equal(mat1_cir2$patch_id, patch_id)
    expect_equal(mat1_cir2$patch_height, 2 * patch_height)
    expect_equal(mat1_cir2$patch_width, 2 * patch_width)
    expect_equal(mat1_cir2$circle_center_x, 2 * circle_x)
    expect_equal(mat1_cir2$circle_center_y, 2 * circle_y)
    expect_equal(mat1_cir2$circle_diameter, 2 * circle_diameter)
    expect_equal(mat1_cir2$circle_area, 4 * circle_area)
})

mat1_cir3 <- rcpp_get_circle(mat = mat1, .5) # half resolution

test_that("rcpp_get_circle handles 0.5 resolution", {
    expect_equal(mat1_cir3$patch_id, patch_id)
    expect_equal(mat1_cir3$patch_height, .5 * patch_height)
    expect_equal(mat1_cir3$patch_width, floor(.5 * patch_width))
    expect_equal(mat1_cir3$circle_center_x, .5 * circle_x)
    expect_equal(mat1_cir3$circle_center_y, .5 * circle_y)
    expect_equal(mat1_cir3$circle_diameter, .5 * circle_diameter)
    expect_equal(mat1_cir3$circle_area, .25 * circle_area)
})
