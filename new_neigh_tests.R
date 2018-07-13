library(raster)
library(landscapemetrics)

landscape_padded <- pad_raster(landscape, pad_raster_value = NA)
landscape_labelled <- cclabel(landscape_padded)

patches_class <- landscape_labelled[[1]]

cells <- raster::Which(!is.na(patches_class), cells = TRUE)
n_cells <- table(raster::values(patches_class))
n_patches <- length(n_cells)

diagonal_matrix <- matrix(c(1, NA, 1,
                            NA, 0, NA,
                            1, NA, 1), 3, 3, byrow = T)

straigth_matrix <- matrix(c(NA, 1, NA,
                            1, 0, 1,
                            NA, 1, NA), 3, 3, byrow = T)

diagonal_neighbours <- raster::adjacent(patches_class,
                                        cells = cells,
                                        directions = diagonal_matrix)

straigth_neighbours <- raster::adjacent(patches_class,
                                        cells = cells,
                                        directions = straigth_matrix)

tb_diagonal <- table(factor(patches_class[diagonal_neighbours[,1]],
                            levels = 1:n_patches),
                     factor(patches_class[diagonal_neighbours[,2]],
                            levels = 1:n_patches))

tb_straight <- table(factor(patches_class[straigth_neighbours[,1]],
                            levels = 1:n_patches),
                     factor(patches_class[straigth_neighbours[,2]],
                            levels = 1:n_patches)) * 2

contiguity <- (((diag(tb_diagonal) + diag(tb_straight) + n_cells) /
                    n_cells) - 1) / 12
