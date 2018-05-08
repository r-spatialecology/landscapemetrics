# Example maps from NLMR -------
set.seed(5)
landscape <- NLMR::nlm_randomcluster(ncol = 30, nrow = 30,
                                   p = 0.4, ai = c(0.25, 0.25, 0.5))
devtools::use_data(landscape, overwrite = TRUE)


landscape_stack <- 1:5 %>%
    purrr::map(function(x){NLMR::nlm_randomcluster(ncol = 30, nrow = 30,
                                                   p = 0.4, ai = c(0.25, 0.25, 0.5))}) %>%
    raster::stack()
devtools::use_data(landscape_stack, overwrite = TRUE)

