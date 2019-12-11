context("options_landscapemetrics")

test_that("option parameters work", {

    options(to_disk = TRUE)

    patches <- get_patches(landscape)

    expect_true(object = getOption("to_disk"))
    expect_false(object = raster::inMemory(patches[[1]]))
})

options(to_disk = NULL)
