library(readr)
library(tidyverse)

# load fragstats results for landscape level
fragstats_landscape <- read_csv("data-raw/fragstats_results_land.txt")

# filter for augusta_nlcd raster
fragstats_landscape_augusta_nlcd <- fragstats_landscape %>%
    filter(str_detect(LID, 'augusta_nlcd'))

# filter for landscape raster
fragstats_landscape_landscape <- fragstats_landscape %>%
    filter(str_detect(LID, 'landscape.tif'))

# filter for podlasie raster
fragstats_landscape_podlasie <- fragstats_landscape %>%
    filter(str_detect(LID, 'podlasie'))

# save --------------------------------------------------------------------
devtools::use_data(fragstats_landscape_augusta_nlcd, overwrite = TRUE)
devtools::use_data(fragstats_landscape_landscape, overwrite = TRUE)
devtools::use_data(fragstats_landscape_podlasie, overwrite = TRUE)
