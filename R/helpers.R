prepare_extras_nonspatial <- function(metrics, landscape, directions, neighbourhood, ordered, base, resolution){
    extras_df_sub <- subset(extras_df, metric %in% metrics)
    extras_list <- unique(extras_df_sub$extras)

    extras <- list()

    if (any(c("enn_patch", "points") %in% extras_list)){
        extras$points <- get_points(landscape)
    }
    if (any(c("area_patches", "enn_patch", "class_patches", "classes")  %in% extras_list)){
        extras$classes <- get_unique_values_int(landscape, verbose = FALSE)
    }
    if (any(c("area_patches", "enn_patch", "class_patches") %in% extras_list)){
        extras$class_patches <- get_class_patches(landscape, extras$classes, directions)
    }
    if ("area_patches" %in% extras_list){
        extras$area_patches <- get_area_patches(extras$class_patches, extras$classes, resolution)
    }
    if ("composition_vector" %in% extras_list){
        extras$composition_vector <- rcpp_get_composition_vector(landscape)
    }
    if (any(c("comp", "neighbor_matrix") %in% extras_list)){
        extras$neighbor_matrix <- rcpp_get_coocurrence_matrix(landscape, directions = as.matrix(neighbourhood))
    }
    if ("comp" %in% extras_list){
        extras$comp <- rcpp_get_entropy(colSums(extras$neighbor_matrix), base)
    }
    if ("cplx" %in% extras_list){
        extras$cplx <- get_complexity(landscape, neighbourhood, ordered, base)
    }
    if ("enn_patch" %in% extras_list){
        extras$enn_patch <- get_enn_patch(extras$classes, extras$class_patches, extras$points)
    }
    return(extras)
}

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

get_points <- function(landscape){
    points <- expand.grid(row = 1:nrow(landscape), col = 1:ncol(landscape))
    points$value <- as.vector(landscape)
    points
}
