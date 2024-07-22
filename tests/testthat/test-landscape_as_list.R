test_that("landscape_as_list returns a list", {
    expect_type(landscape_as_list(landscape), "list")
    expect_type(landscape_as_list(landscape_stack), "list")
    expect_type(landscape_as_list(landscape_list), "list")
    expect_type(landscape_as_list(landscape_matrix), "list")
    expect_type(landscape_as_list(terra::wrap(landscape)), "list")

})
