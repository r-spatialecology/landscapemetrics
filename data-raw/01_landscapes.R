# Create example datasets -------------------------------------------------
set.seed(2018-05-12)
single_landscape_create = function(x) {
    NLMR::nlm_randomcluster(ncol = 30, nrow = 30, p = 0.4, ai = c(0.25, 0.25, 0.5),
                            rescale = FALSE)
}
# Example maps from NLMR --------------------------------------------------
landscape <- single_landscape_create()
devtools::use_data(landscape, overwrite = TRUE)

# raster stack ------------------------------------------------------------
# (a) input type (e.g. RasterLayer vs RasterStack),
# (b) number of classes (e.g. binary vs 5-classes),
# (c) data availability (e.g. full data vs some missing values),
# (d) distribution of values (e.g. random vs aggregated).
library(tibble)
library(purrr)
library(tidyr)
library(landscapetools)
library(raster)
library(NLMR)
set.seed(2018-05-15)

nlm_create = function(roughness, weighting){
    nlm_mpd(ncol = 33, nrow = 33, roughness = roughness, rescale = TRUE) %>%
        util_classify(weighting = weighting)
}

param_df = expand.grid(roughness = c(0.3, 0.7),
                       weighting = list(c(0.2, 0.8), c(0.2, 0.3, 0.5))) %>%
    as.tibble()
nlm_list = param_df %>% pmap(nlm_create)

# missing values case -----------------------------------------------------
nlm_5 = nlm_list[[4]]
nlm_5[nlm_5 == 1] = NA

# random case -------------------------------------------------------------
nlm_6 = nlm_random(ncol = 33, nrow = 33) %>%
    util_classify(weighting = c(0.25, 0.25, 0.5))

# combine all -------------------------------------------------------------
landscape_stack = stack(c(nlm_list, nlm_5, nlm_6))
names(landscape_stack) = paste0("landscape", 1:6)
# plot(landscape_stack)

# save --------------------------------------------------------------------
devtools::use_data(landscape_stack, overwrite = TRUE)

# plot --------------------------------------------------------------------
# library(rasterVis)
# library(ggplot2)
#
# gplot(landscape_stack) +
#     geom_tile(aes(fill = as.factor(value))) +
#     facet_wrap(~variable) +
#     scale_fill_brewer(name = "Class: ", type = "qual", palette = "Set2", na.value = "grey50") +
#     theme_void()
