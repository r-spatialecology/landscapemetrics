context("landscape_as_list")

test_that("landscape_as_list returns a list", {
    expect_is(landscape_as_list(landscape), "list")
    expect_is(landscape_as_list(landscape_stack), "list")
    expect_is(landscape_as_list(landscape_list), "list")
    expect_is(landscape_as_list(landscape_matrix), "list")

    })
