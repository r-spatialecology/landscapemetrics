library(NLMR)
library(terra)
library(usethis)

#### Create example datasets ####
set.seed(2018-05-12)

single_landscape_create = function(x) {
    NLMR::nlm_randomcluster(ncol = 30, nrow = 30, p = 0.4, ai = c(0.25, 0.25, 0.5),
                            rescale = FALSE)
}

# Example maps from NLMR
landscape <- terra::wrap(terra::rast(single_landscape_create()))

usethis::use_data(landscape, overwrite = TRUE)

#### Example for background vignette ####

landscape_continous <- NLMR::nlm_fbm(100, 100, fract_dim = 1.3, user_seed = 5)

landscape_categorical <- landscapetools::util_classify(x = landscape_continous,
                                                       weighting = c(0.2, 0.2, 0.2, 0.2, 0.2))

landscape_background <- list("a) Continous Landscape" = terra::wrap(terra::rast(landscape_continous)),
                             "b) Categorical Landscape" = terra::wrap(terra::rast(landscape_categorical)))

usethis::use_data(landscape_background, overwrite = TRUE, internal = TRUE)
