library(landscapemetrics)
library(supercells)
library(motif)
library(sf)
library(terra)

# prepare example data ----------------------------------------------------
my_cat_raster = rast(landscapemetrics::augusta_nlcd)
my_cat_raster2 = lsp_signature(my_cat_raster, type = "cove", window = 5, ordered = FALSE)
my_cat_raster2 = lsp_add_terra(my_cat_raster2)
irregular_areas = supercells(my_cat_raster2[[-c(1, 2)]], k = 30,
                                   compactness = 0.5, clean = TRUE,
                                   dist_fun = "jensen-shannon")

plot(my_cat_raster, type = "classes")
plot(vect(irregular_areas), add = TRUE)
usethis::use_data(irregular_areas)
