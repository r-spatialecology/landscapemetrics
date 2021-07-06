library(readr)
library(tidyverse)
library(usethis)

# load fragstats results for class level
fragstats_class <- read_csv("data-raw/fragstats_results_class.txt")

fragstats_class$TYPE <-
    as.numeric(str_split(fragstats_class$TYPE, pattern = "_",
                         simplify = TRUE)[, 2])

# filter for augusta_nlcd raster
fragstats_class_augusta_nlcd <- fragstats_class %>%
    filter(str_detect(LID, 'augusta_nlcd'))

# filter for landscape raster
fragstats_class_landscape <- fragstats_class %>%
    filter(str_detect(LID, 'landscape.tif'))

# filter for podlasie raster
fragstats_class_podlasie <- fragstats_class %>%
    filter(str_detect(LID, 'podlasie'))

# save --------------------------------------------------------------------
usethis::use_data(fragstats_class_augusta_nlcd, overwrite = TRUE)
usethis::use_data(fragstats_class_landscape, overwrite = TRUE)
usethis::use_data(fragstats_class_podlasie, overwrite = TRUE)
