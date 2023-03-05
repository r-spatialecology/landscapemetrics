disaggregate_sp_tmp = function(x){
    if (requireNamespace("terra", quietly = TRUE)) {
        x = methods::as(terra::disagg(terra::vect(x)), "Spatial")
    } else {
        warning("Package 'terra' is not installed. Please make sure polygons are disaggregated.", call. = FALSE)
    }
    return(x)
}

