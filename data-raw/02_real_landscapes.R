library(raster)

# nlcd2011 ----------------------------------------------------------------
# https://www.mrlc.gov/nlcd2011.php ---------------------------------------

nlcd_bbox = extent(c(1249650, 1270000, 1246800, 1260000))
nlcd_filepath = "your_path"

augusta_nlcd = crop(raster(nlcd_filepath), nlcd_bbox)

plot(augusta_nlcd)
devtools::use_data(augusta_nlcd)

# ESACCI-LC 2015 ----------------------------------------------------------
# http://maps.elie.ucl.ac.be/CCI/viewer/ ----------------------------------

ccilc_bbox = extent(c(22.23, 23.5, 52.8, 53.83))
ccilc_filepath = "your_path"

podlasie_ccilc = crop(raster(ccilc_filepath, band = 24), ccilc_bbox)

plot(podlasie_ccilc)
devtools::use_data(podlasie_ccilc)
