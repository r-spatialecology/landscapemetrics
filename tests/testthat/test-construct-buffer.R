context("construct_buffer")

test_that("construct_buffer is typestable", {
    expect_is(construct_buffer(points_matrix, shape = "circle", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_sp, shape = "square", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_spdf, shape = "circle", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_point, shape = "square", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_multipoint, shape = "circle", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_sfc, shape = "square", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_sf, shape = "circle", size = 5), "SpatialPolygons")
})

