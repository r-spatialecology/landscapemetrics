context("get_circumscribingcircle")

class_1 <- get_patches(landscape, class = 1)[[1]]
class_1_irr <- get_patches(landscape_diff_res, class = 1)[[1]]

test_that("get_circumscribingcircle has one radius for each patch", {

    circle <- get_circumscribingcircle(class_1)
    expect_equal(nrow(circle),
                     expected =  max(get_unique_values(class_1)[[1]]))
})

test_that("get_circumscribingcircle has two rows and is tibble", {

    circle <- get_circumscribingcircle(class_1)
    expect_is(circle, class = "tbl")
    expect_equal(ncol(circle), expected = 2)

})

test_that("get_circumscribingcircle works for irregular raster", {

    circle <- get_circumscribingcircle(class_1_irr)

    expect_is(circle, class = "tbl")
    expect_equal(ncol(circle), expected = 2)
    expect_equal(nrow(circle),
                 expected =  max(get_unique_values(class_1_irr)[[1]]))

})
