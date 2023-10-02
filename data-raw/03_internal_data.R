library(motif)
library(NLMR)
library(landscapemetrics)
library(landscapetools)
library(sf)
library(supercells)
library(terra)
library(usethis)

#### Example landscape background ####

set.seed(2023-05-23)

landscape_continous <- NLMR::nlm_fbm(100, 100, fract_dim = 1.3, user_seed = 5)

landscape_categorical <- landscapetools::util_classify(x = landscape_continous,
                                                       weighting = c(0.2, 0.2, 0.2, 0.2, 0.2))

landscape_background <- list("continous" = terra::wrap(terra::rast(landscape_continous)),
                             "categorical" = terra::wrap(terra::rast(landscape_categorical)))

#### Irregular areas ####

# prepare example data ----------------------------------------------------
my_cat_raster = rast(landscapemetrics::augusta_nlcd)
my_cat_raster2 = lsp_signature(my_cat_raster, type = "cove", window = 5, ordered = FALSE)
my_cat_raster2 = lsp_add_terra(my_cat_raster2)
irregular_areas = supercells(my_cat_raster2[[-c(1, 2)]], k = 30,
                                   compactness = 0.5, clean = TRUE,
                                   dist_fun = "jensen-shannon")

irregular_areas = irregular_areas[1]

#### Fragstats data ####

# import and reshape FRAGSTATS v2.0 results

fragstats_patch <- c(landscape = "data-raw/landscape.patch", augusta_nlcd = "data-raw/augusta_nlcd.patch") |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID")

fragstats_class <- c(landscape = "data-raw/landscape.class", augusta_nlcd = "data-raw/augusta_nlcd.class") |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID")

fragstats_landscape <- c(landscape = "data-raw/landscape.land", augusta_nlcd = "data-raw/augusta_nlcd.land") |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID")

fs_data <- list(patch = fragstats_patch, class = fragstats_class, landscape = fragstats_landscape)

#### Combine data

internal_data <- list(landscape_background = landscape_background,
                      irregular_areas = irregular_areas,
                      fs_data = fs_data)

usethis::use_data(internal_data, overwrite = TRUE, internal = TRUE)
