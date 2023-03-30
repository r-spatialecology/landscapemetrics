# get patches for class 1
class_1 <- get_patches(landscape, class = 1)[[1]][[1]]

all_classes <- get_patches(landscape)[[1]]

test_that("get_nearestneighbour works for all data types", {

    raster_layer <- get_nearestneighbour(class_1)
    raster_stack <- get_nearestneighbour(c(all_classes))
    raster_list <- get_nearestneighbour(all_classes)

    expect_s3_class(raster_layer, "tbl_df")
    expect_s3_class(raster_stack, "tbl_df")
    expect_s3_class(raster_list, "tbl_df")
})

test_that("get_nearestneighbour returns value for each patch", {

    np <- lsm_l_np(class_1)
    raster_layer <- get_nearestneighbour(class_1)

    expect_true(object = np$value == nrow(raster_layer))
})

test_that("get_nearestneighbour can return focal and neighbour ID", {

    raster_layer <- get_nearestneighbour(class_1, return_id = TRUE)

    expect_true(object = ncol(raster_layer) == 4)
    expect_true(object = all(raster_layer$id != raster_layer$id_neighbour))
})
