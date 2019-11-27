context("test empty raster")

# prepare data ------------------------------------------------------------
r <- raster(matrix(c(rep(NA, 18), 1:18), nrow = 6))

p <- raster(matrix(c(rep(1, 18), rep(2, 18)), nrow = 6))
p_sp <- rasterToPolygons(p, dissolve = TRUE)

# plot(r)
# plot(p_sp, add = TRUE, lwd = 5)

# calculate metrics -------------------------------------------------------
expect_warning(my_metric <- sample_lsm(r, p_sp, metric = "shdi"))
expect_true(is.na(my_metric$value[1]))
