# nolint start
context("metric_area")

x <- nlm_random(100, 100)
y <- c(0.5, 0.25, 0.25)
z <- util_classify(x, y)



test_that("metric_area is a good boy", {
  expect_that(metric_area(z), is_a("list"))
  expect_equal(length(metric_area(z)), 2)
  expect_equal(length(metric_area(z, poi = 1)), 2)
  })

test_that("metric_area calculates correct", {

  z_area <- metric_area(z)

  expect_that(metric_area(z), is_a("list"))
  expect_equal(z_area[[1]][1,1][[1]], 4999)
  expect_equal(z_area[[1]][2,1][[1]], 2500)
  expect_equal(z_area[[1]][3,1][[1]], 2501)

  })

test_that("metric_area calculates correct", {

  z_area <- metric_area(z)

  expect_that(metric_area(z), is_a("list"))
  expect_equal(z_area[[2]][1,1][[1]], 0.4999)
  expect_equal(z_area[[2]][2,1][[1]], 0.2500)
  expect_equal(z_area[[2]][3,1][[1]], 0.2501)

})

# nolint end
