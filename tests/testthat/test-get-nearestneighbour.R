context("get_nearestneighbour")


# get patches for class 1 from testdata as raster
class_1 <- get_patches(landscape,1)[[1]]
# calculate the distance between patches
nn_rast <- get_nearestneighbour(class_1)
# do the same with a 3 column matrix (x,y,id)
class_1_matrix <- raster_to_points(class_1, return_NA = FALSE)
nn_mat<- get_nearestneighbour(class_1_matrix)

test_that("get_adjacencies runs and returns a matrix", {
    expect_is(nn_rast, "tbl_df")
    expect_is(nn_mat, "tbl_df")

    expect_true(nn_rast[1,2] == 7)
    expect_true(nn_mat[1,2] == 7)
})
