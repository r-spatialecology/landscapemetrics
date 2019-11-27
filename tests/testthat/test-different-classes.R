context("test different classes")

landscape_raster = landscape
landscape_stack = raster::stack(landscape)
landscape_brick = raster::brick(landscape)
# landscape_stars = stars::st_as_stars(landscape)
landscape_list = list(landscape)

ent_landscape_raster = lsm_l_ent(landscape_raster)

expect_equal(ent_landscape_raster, lsm_l_ent(landscape_stack))
expect_equal(ent_landscape_raster, lsm_l_ent(landscape_brick))
# expect_equal(ent_landscape_raster, lsm_l_ent(landscape_stars))
expect_equal(ent_landscape_raster, lsm_l_ent(landscape_list))
