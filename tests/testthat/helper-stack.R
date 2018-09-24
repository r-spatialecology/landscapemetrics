# raster stack ------------------------------------------------------------
# (a) input type (e.g. RasterLayer vs RasterStack),
# (b) number of classes (e.g. binary vs 5-classes),
# (c) data availability (e.g. full data vs some missing values),
# (d) distribution of values (e.g. random vs aggregated).
set.seed(2018-05-15)

nlm_create <-  function(roughness, weighting){
    NLMR::nlm_mpd(ncol = 33, nrow = 33, roughness = roughness, rescale = TRUE, verbose = FALSE) %>%
        landscapetools::util_classify(weighting = weighting)
}

param_df <-
    tibble::as.tibble(expand.grid(roughness = c(0.3, 0.7),
                                  weighting = list(c(0.2, 0.8), c(0.2, 0.3, 0.5))) )

nlm_list <- purrr::pmap(param_df, nlm_create)

# missing values case -----------------------------------------------------
nlm_5 <-  nlm_list[[4]]
nlm_5[nlm_5 == 1]  <-  NA

# random case -------------------------------------------------------------
nlm_6 <-
    landscapetools::util_classify(NLMR::nlm_random(ncol = 33, nrow = 33), weighting = c(0.25, 0.25, 0.5))

# combine all -------------------------------------------------------------
landscape_stack <- raster::stack(c(nlm_list, nlm_5, nlm_6))
names(landscape_stack)<- paste0("landscape", 1:6)
