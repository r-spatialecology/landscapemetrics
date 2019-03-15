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
    check_tibble <- check_landscape(landscape_stack)
    expect_true(all(check_tibble$OK == cli::symbol$circle_question_mark))
})

test_that("check_landscape works (maybe) for landscape brick", {
    check_tibble <- check_landscape(landscape_brick)
    expect_true(all(check_tibble$OK == cli::symbol$circle_question_mark))
})

test_that("check_landscape works (maybe) for landscape list", {
    check_tibble <- check_landscape(landscape_list)
    expect_true(all(check_tibble$OK == cli::symbol$circle_question_mark))
})

test_that("check_landscape works does not work for double values", {

    landscape[] <- 1.5

    check_tibble <- check_landscape(landscape)

    expect_true(all(check_tibble$OK == cli::symbol$cross))
})

test_that("check_landscape works return warning for > 30 classes", {

    augusta_nlcd[] <- sample(1:35, size = 298760, replace = TRUE)

    check_tibble <- check_landscape(augusta_nlcd)

    expect_true(all(check_tibble$OK == cli::symbol$circle_question_mark))
})
