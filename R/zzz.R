.onLoad <- function(libname, pkgname){

    packageStartupMessage("Soon and starting from v2.0.0 landscapemetrics will not support the 'raster' or 'sp' package
    anymore. They will be replaced with 'terra' and 'sf', respectively. More information
    about the 'terra' package can be found here: https://rspatial.org/index.html.")

}
