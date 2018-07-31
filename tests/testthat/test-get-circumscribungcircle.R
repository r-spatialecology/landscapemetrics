context("get_circumscribingcircle")


# get patches for class 1 from testdata as raster
class_1 <- get_patches(landscape,1)[[1]]
# calculate the distance between patches
cc_rast <- get_circumscribingcircle(class_1)
# do the same with a 3 column matrix (x,y,id)
class_1_matrix <- raster::rasterToPoints(class_1)
cc_mat<- get_circumscribingcircle(class_1_matrix, resolution = 1)

test_that("get_adjacencies runs and returns a matrix", {
    expect_is(cc_rast, "tbl_df")
    expect_is(cc_mat, "tbl_df")

    expect_true(round(cc_rast$dist[[1]],2) == 6.28)
    expect_true(round(cc_mat$dist[[1]],2) == 6.28)

    expect_error(get_circumscribingcircle(class_1_matrix))
})
