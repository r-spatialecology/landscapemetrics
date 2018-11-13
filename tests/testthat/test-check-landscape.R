context("check_landscape")

test_that("calculate_lsm can take name argument", {
    check_tibble <- check_landscape(augusta_nlcd)
    expect_true(check_tibble$OK == "✔")
})


test_that("calculate_lsm can take name argument", {
    check_tibble <- check_landscape(podlasie_ccilc)
    expect_true(check_tibble$OK == "✖")
})

test_that("calculate_lsm can take name argument", {
    check_tibble <- check_landscape(landscape)
    expect_true(check_tibble$OK == "?⃝")
})

test_that("calculate_lsm can take name argument", {
    check_tibble <- check_landscape(raster::stack(landscape, landscape))
    expect_true(all(check_tibble$OK == "?⃝"))
})
