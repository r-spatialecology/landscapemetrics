context("check_landscape")

test_that("check_landscape works for augusta_nlcd", {
    check_tibble <- check_landscape(augusta_nlcd)
    expect_true(check_tibble$OK == cli::symbol$tick)
})


test_that("check_landscape works (not) for podlasie_ccilc", {
    check_tibble <- check_landscape(podlasie_ccilc)
    expect_true(check_tibble$OK == cli::symbol$cross)
})

test_that("check_landscape works (maybe) for landscape", {
    check_tibble <- check_landscape(landscape)
    expect_true(check_tibble$OK == cli::symbol$circle_question_mark)
})

test_that("check_landscape works (maybe) for landscape stack", {
    check_tibble <- check_landscape(raster::stack(landscape, landscape))
    expect_true(all(check_tibble$OK == cli::symbol$circle_question_mark))
})
