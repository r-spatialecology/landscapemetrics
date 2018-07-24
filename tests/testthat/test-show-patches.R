context("show_patches")

patches_plot <- show_patches(landscape)

test_that("show_patches returns a plot", {
    expect_equal(class(patches_plot), c("gg","ggplot"))
})
