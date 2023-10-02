library(raster)
library(terra)
library(usethis)


# nlcd2011 ----------------------------------------------------------------
# https://www.mrlc.gov/nlcd2011.php ---------------------------------------

nlcd_bbox = extent(c(1249650, 1270000, 1246800, 1260000))
nlcd_filepath = "your_path"
augusta_nlcd = terra::wrap(terra::crop(terra::rast(nlcd_filepath), nlcd_bbox))

usethis::use_data(augusta_nlcd, overwrite = TRUE)

# ESACCI-LC 2015 ----------------------------------------------------------
# http://maps.elie.ucl.ac.be/CCI/viewer/ ----------------------------------

ccilc_bbox = extent(c(22.23, 23.5, 52.8, 53.83))
ccilc_filepath = "your_path"
podlasie_ccilc = terra::wrap(terra::crop(terra::rast(ccilc_filepath), ccilc_bbox))

usethis::use_data(podlasie_ccilc, overwrite = TRUE)

# library(terra)
# library(landscapemetrics)
#
# augusta_nlcd <- terra::rast(landscapemetrics::augusta_nlcd)
# podlasie_ccilc <- terra::rast(landscapemetrics::podlasie_ccilc)
#
# writeRaster(x = augusta_nlcd, filename = "C:/Users/hesselbarth/Desktop/augusta_nlcd.tif", overwrite = TRUE)
# writeRaster(x = podlasie_ccilc, filename = "C:/Users/hesselbarth/Desktop/podlasie_ccilc.tif", overwrite = TRUE)
