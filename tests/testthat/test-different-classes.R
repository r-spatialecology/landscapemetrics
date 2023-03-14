context("test different classes")

landscape_stack = c(landscape)
# landscape_stars = stars::st_as_stars(landscape)
landscape_list = list(landscape)

ent_landscape = lsm_l_ent(landscape)

expect_equal(ent_landscape, lsm_l_ent(landscape_stack))
# expect_equal(ent_landscape_raster, lsm_l_ent(landscape_stars))
expect_equal(ent_landscape, lsm_l_ent(landscape_list))

