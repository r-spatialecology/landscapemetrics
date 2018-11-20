lsm_get_coocurrence_matrix <- function(x, directions) {
    # todo: ceck if x is raster obj and directions numeric matrix
    ncol <- ncol(x)
    nrow <- nrow(x)
    classes <- raster::unique(x)
    mat <- raster::values(x) # eats quite some memory
    lsm_get_coocurrence_matrix_(mat, directions, ncol, nrow, classes)
}


# does not use any additional memory, might be interesting for large rasters
lsm_get_coocurrence_matrix_ <- function(x, directions, ncol, nrow, classes) {
    # todo: ceck if x is numeric matrix obj and directions numeric matrix ....
    rcpp_get_coocurrence_matrix2(x, directions, ncol, nrow, classes)
}
