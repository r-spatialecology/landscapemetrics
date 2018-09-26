# Create example datasets -------------------------------------------------
set.seed(2018-05-12)
single_landscape_create = function(x) {
    NLMR::nlm_randomcluster(ncol = 30, nrow = 30, p = 0.4, ai = c(0.25, 0.25, 0.5),
                            rescale = FALSE)
}
# Example maps from NLMR --------------------------------------------------
landscape <- single_landscape_create()
devtools::use_data(landscape, overwrite = TRUE)
