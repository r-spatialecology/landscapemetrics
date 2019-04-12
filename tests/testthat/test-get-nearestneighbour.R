context("get_nearestneighbour")


# get patches for class 1 from testdata as raster
class_1 <- get_patches(landscape,1)[[1]]
# calculate the distance between patches
nn_rast <- get_nearestneighbour(class_1)
# do the same with a 3 column matrix (x,y,id)
class_1_matrix <- raster_to_points(class_1, return_NA = FALSE)
nn_mat <- get_nearestneighbour(class_1_matrix[, 2:4])

test_that("get_adjacencies runs and returns a matrix", {
    expect_is(nn_rast, "tbl_df")
    expect_is(nn_mat, "tbl_df")

    expect_true(nn_rast[1, 3] == 7)
    expect_true(nn_mat[1, 3] == 7)
})

test_that("get_adjacencies runs for all data type", {

    result_stack <- get_nearestneighbour(landscape_stack)
    result_brick <- get_nearestneighbour(landscape_brick)
    result_list <- get_nearestneighbour(landscape_list)

    expect_is(result_stack, "tbl_df")
    expect_is(result_brick, "tbl_df")
    expect_is(result_list, "tbl_df")

    expect_true(all(c(1, 2) %in% result_stack$layer))
    expect_true(all(c(1, 2) %in% result_brick$layer))
    expect_true(all(c(1, 2) %in% result_list$layer))
})
