context("get_circumscribingcircle")

test_that("get_circumscribingcircle has one radius for each patch", {

    circle <- get_circumscribingcircle(landscape)

    expect_equal(nrow(circle),
                 expected =  lsm_l_np(landscape)$value)

    expect_true(object = all(all(circle$value != 0),
                             all(!is.na(circle$value)),
                             all(!is.infinite(circle$value))))

    expect_is(circle, class = "tbl")
})

test_that("get_circumscribingcircle has one radius for each class", {

    circle <- get_circumscribingcircle(landscape, level = "class")

    expect_equal(nrow(circle),
                 expected = lsm_l_pr(landscape)$value)

    expect_true(object = all(all(circle$value != 0),
                             all(!is.na(circle$value)),
                             all(!is.infinite(circle$value))))
})

test_that("get_circumscribingcircle works for all data type matrix", {

   result_stack <- get_circumscribingcircle(landscape_stack, level = "class")
   result_list <- get_circumscribingcircle(landscape_list, level = "class")

   expect_equal(object = unique(result_stack$layer), expected = c(1, 2))
   expect_equal(object = unique(result_list$layer), expected = c(1, 2))

   expect_equal(nrow(result_stack), expected = sum(lsm_l_pr(landscape_stack)$value))
   expect_equal(nrow(result_list), expected = sum(lsm_l_pr(landscape_list)$value))
})

test_that("get_circumscribingcircle returns errors", {

    expect_error(object = get_circumscribingcircle(landscape, level = "landscape"),
                 regexp = "The 'level' argument must be either 'patch' or 'class'.")

    expect_error(object = get_circumscribingcircle(landscape_diff_res),
                 regexp = "The area of the circumscribing circle is currently only implemented for equal resolutions.")

    expect_error(object = get_circumscribingcircle(list(terra::as.matrix(landscape, wide = TRUE))),
                 regexp = "Please provide a 'SpatRaster', 'stars'-object, or a list with 'SpatRaster'.")
})
