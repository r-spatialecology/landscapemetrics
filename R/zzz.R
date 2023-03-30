.onAttach <- function(libname, pkgname){

    packageStartupMessage("Starting from v2.0.0, landscapemetrics does not support the 'raster' or 'sp' packages.
    They are replaced by 'terra' and 'sf', respectively. More information
    about the 'terra' package can be found here: https://rspatial.org/index.html.")

}
