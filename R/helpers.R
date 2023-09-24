get_class_patches <- function(landscape, classes, directions){
    class_patches <- lapply(classes, function(patches_class){
                landscape_labeled <- get_patches_int(landscape,
                                             class = patches_class,
                                             directions = directions)[[1]]
    })
    names(class_patches) <- classes
    return(class_patches)
}

get_area_patches <- function(class_patches, classes, resolution){
    factor_ha <- prod(resolution) / 10000
    area_patches <- lapply(classes, function(patches_class){
        landscape_labeled <- class_patches[[as.character(patches_class)]]
        area_patch_ij <- rcpp_get_composition_vector(x = landscape_labeled) * factor_ha

    })
    names(area_patches) <- classes
    return(area_patches)
}

get_complexity <- function(landscape, neighbourhood, ordered, base){
    coh <- rcpp_get_coocurrence_vector(landscape,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)
    cplx <- rcpp_get_entropy(coh, base)
    return(cplx)
}

get_enn_patch <- function(classes, class_patches, points){
    enn_patch <- do.call(rbind,
                         lapply(classes, function(patches_class) {

                             # get connected patches
                            landscape_labeled <- class_patches[[as.character(patches_class)]]

                             # get number of patches
                             np_class <- max(landscape_labeled, na.rm = TRUE)

                             # ENN doesn't make sense if only one patch is present
                             if (np_class == 1) {

                                 enn <- tibble::tibble(class = patches_class,
                                                       dist = as.double(NA))

                                 if (verbose) {
                                     warning(paste0("Class ", patches_class,
                                                    ": ENN = NA for class with only 1 patch."),
                                             call. = FALSE)
                                 }
                             } else {

                                 enn <- get_nearestneighbour_calc(landscape = landscape_labeled,
                                                                  return_id = FALSE,
                                                                  points = points)
                             }

                             tibble::tibble(class = patches_class,
                                            value = enn$dist)
                         })
    )
}