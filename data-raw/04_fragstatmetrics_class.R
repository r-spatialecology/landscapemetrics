library(readr)
library(tidyverse)

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

# filter for landscape raster stack
fragstats_class_landscapestack <- fragstats_class %>%
    filter(str_detect(LID, 'landscape_stack'))

# filter for podlasie raster
fragstats_class_podlasie <- fragstats_class %>%
    filter(str_detect(LID, 'podlasie'))

# save --------------------------------------------------------------------
devtools::use_data(fragstats_class_augusta_nlcd, overwrite = TRUE)
devtools::use_data(fragstats_class_landscape, overwrite = TRUE)
devtools::use_data(fragstats_class_landscapestack, overwrite = TRUE)
devtools::use_data(fragstats_class_podlasie, overwrite = TRUE)
