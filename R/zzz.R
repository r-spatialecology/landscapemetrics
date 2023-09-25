.onAttach <- function(libname, pkgname){

    packageStartupMessage(
    "Starting from v2.0, landscapemetrics uses the 'terra' and 'sf' instead of 'raster' and 'sp'.
    This might results in a slightly different behavior of the code.
    More information about the 'terra' package can be found here: https://rspatial.org/index.html."
    )
}
