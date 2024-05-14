#' prepare_extras
#'
#' @description Prepare an extras object
#'
#' @param metrics A vector with metric abbreviations
#' @param landscape_mat A matrix object
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours: 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured. The default is "log2",
#' which compute entropy in "bits". "log" and "log10" can be also used.
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#'
#' @details
#' Wrapper around terra::xyFromCell and terra::getValues to get raster_to_points
#' function including NA values
#'
#' @return A list with zero or more of the following components:
#' \itemize{
#'  \item points: matrix with three columns: col, row, value
#'  \item classes: vector with unique values
#'  \item class_patches: list with matrices of patches for each class
#'  \item area_patches: list with vectors of areas of patches for each class
#'  \item composition_vector: vector with the number of cells for each class
#'  \item neighbor_matrix: matrix with the number of cell pairs for each class
#'  \item comp: entropy of the neighbor_matrix
#'  \item cplx: complexity of the landscape
#'  \item enn_patch: matrix with the euclidean nearest neighbour distance for each patch
#' }
#'
#' @seealso
#' \code{\link{get_points}},
#' \code{\link{get_class_patches}},
#' \code{\link{get_area_patches}},
#' \code{\link{get_complexity}},
#' \code{\link{get_enn_patch}}
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' prepare_extras("lsm_l_ent", landscape_mat, neighbourhood = 4, base = "log2")
#'
#' \dontrun{
#' metrics = list_lsm()$function_name
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' prepare_extras(metrics, landscape_mat, directions = 8, neighbourhood = 4,
#'                ordered = FALSE, base = "log2", resolution = terra::res(landscape))
#' }
#'
#' @keywords internal
#'
#' @export
prepare_extras <- function(metrics, landscape_mat, directions, neighbourhood, ordered, base, resolution){
    extras_df_sub <- extras_df[extras_df$metric %in% metrics, ]
    extras_list <- unique(extras_df_sub$extras)

    extras <- list()

    if (any(c("enn_patch", "points") %in% extras_list)){
        extras$points <- get_points(landscape_mat, resolution)
    }
    if (any(c("area_patches", "enn_patch", "class_patches", "perimeter_patch", "classes")  %in% extras_list)){
        extras$classes <- get_unique_values_int(landscape_mat, verbose = FALSE)
    }
    if (any(c("area_patches", "enn_patch", "perimeter_patch", "class_patches") %in% extras_list)){
        extras$class_patches <- get_class_patches(landscape_mat, extras$classes, directions)
    }
    if ("area_patches" %in% extras_list){
        extras$area_patches <- get_area_patches(extras$class_patches, extras$classes, resolution)
    }
    if ("composition_vector" %in% extras_list){
        extras$composition_vector <- rcpp_get_composition_vector(landscape_mat)
    }
    if (any(c("comp", "neighbor_matrix") %in% extras_list)){
        extras$neighbor_matrix <- rcpp_get_coocurrence_matrix(landscape_mat, directions = as.matrix(neighbourhood))
    }
    if ("comp" %in% extras_list){
        extras$comp <- rcpp_get_entropy(colSums(extras$neighbor_matrix), base)
    }
    if ("cplx" %in% extras_list){
        extras$cplx <- get_complexity(landscape_mat, neighbourhood, ordered, base)
    }
    if ("enn_patch" %in% extras_list){
        extras$enn_patch <- get_enn_patch(extras$classes, extras$class_patches, extras$points)
    }
    if ("perimeter_patch" %in% extras_list){
        extras$perimeter_patch <- get_perimeter_patch(extras$classes, extras$class_patches, resolution)
    }
    return(extras)
}

#' get_class_patches
#'
#' @description Get patches for each class
#'
#' @param landscape_mat A matrix object
#' @param classes A vector with unique values (output of get_unique_values_int)
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' Calculate patches for each class
#'
#' @return list with matrices of patches for each class
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' classes <- landscapemetrics:::get_unique_values_int(landscape_mat)
#' class_patches <- get_class_patches(landscape_mat, classes, directions = 8)
#'
#' @keywords internal
#'
#' @export
get_class_patches <- function(landscape_mat, classes, directions){
    class_patches <- lapply(classes, function(patches_class){
                landscape_labeled <- get_patches_int(landscape_mat,
                                             class = patches_class,
                                             directions = directions)[[1]]
    })
    names(class_patches) <- classes
    return(class_patches)
}

#' get_area_patches
#'
#' @description Get areas of patches for each class
#'
#' @param class_patches A list with matrices of patches for each class (output of get_class_patches)
#' @param classes A vector with unique values (output of get_unique_values_int)
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#'
#' @details
#' Calculate areas of patches for each class
#'
#' @return list with vectors of areas of patches for each class
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' classes <- landscapemetrics:::get_unique_values_int(landscape_mat)
#' class_patches <- get_class_patches(landscape_mat, classes, directions = 8)
#' area_patches <- get_area_patches(class_patches, classes, resolution = terra::res(landscape))
#'
#' @keywords internal
#'
#' @export
get_area_patches <- function(class_patches, classes, resolution){
    factor_ha <- prod(resolution) / 10000
    area_patches <- lapply(classes, function(patches_class){
        landscape_labeled <- class_patches[[as.character(patches_class)]]
        area_patch_ij <- rcpp_get_composition_vector(x = landscape_labeled) * factor_ha
    })
    names(area_patches) <- classes
    return(area_patches)
}

#' get_complexity
#'
#' @description Calculate complexity of the landscape
#'
#' @param landscape_mat A matrix object
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours: 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' @param base The unit in which entropy is measured. The default is "log2",
#'
#' @details
#' Calculate complexity of the landscape: entropy of the co-occurrence matrix
#'
#' @return matrix
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' get_complexity(landscape_mat, neighbourhood = 4, ordered = TRUE, base = "log2")
#'
#' @keywords internal
#'
#' @export
get_complexity <- function(landscape_mat, neighbourhood, ordered, base){
    coh <- rcpp_get_coocurrence_vector(landscape_mat,
                                       directions = as.matrix(neighbourhood),
                                       ordered = ordered)
    cplx <- rcpp_get_entropy(coh, base)
    return(cplx)
}

#' get_points
#'
#' @description Raster to col, row, value
#'
#' @param landscape_mat A matrix object
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#'
#' @details
#' The col and row values are multiplied by the resolution to get the (internal) coordinates of the points.
#'
#' @return matrix with three columns: col, row, value
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' resolution <- terra::res(landscape)
#' get_points(landscape_mat, resolution)
#'
#' @keywords internal
#'
#' @export
get_points <- function(landscape_mat, resolution){
    points <- expand.grid(col = seq_len(ncol(landscape_mat)),
                          row = seq_len(nrow(landscape_mat)))
    points <- mapply(FUN = `*`, points, resolution)
    points <- cbind(points, value = as.vector(landscape_mat))
    points
}

#' get_enn_patch
#'
#' @description Euclidean Nearest-Neighbor Distance
#'
#' @param classes A vector with unique values (output of get_unique_values_int)
#' @param class_patches A list with matrices of patches for each class (output of get_class_patches)
#' @param points A matrix with three columns: col, row, value (output of get_points)
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#' @param verbose A logical indicating whether to print warnings
#'
#' @details
#' Calculate Euclidean Nearest-Neighbor Distance for each patch in each class
#'
#' @return tibble with two columns: class, value
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' classes <- landscapemetrics:::get_unique_values_int(landscape_mat)
#' class_patches <- get_class_patches(landscape_mat, classes, directions = 8)
#' points <- get_points(landscape_mat, terra::res(landscape))
#' enns <- get_enn_patch(classes, class_patches, points, terra::res(landscape))
#'
#' @keywords internal
#'
#' @export
get_enn_patch <- function(classes, class_patches, points, resolution, verbose = FALSE){
    enn_patch <- do.call(rbind, lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- class_patches[[as.character(patches_class)]]

        # get number of patches
        np_class <- max(landscape_labeled, na.rm = TRUE)

        # ENN doesn't make sense if only one patch is present
        if (np_class == 1) {

            enn <- tibble::new_tibble(list(class = patches_class, dist = as.double(NA)))

            if (verbose) {
                warning(paste0("Class ", patches_class, ": ENN = NA for class with only 1 patch."),
                        call. = FALSE)
            }
        } else {

            enn <- get_nearestneighbour_calc(landscape = landscape_labeled, return_id = FALSE,
                                             resolution = resolution,
                                             points = points)
        }

        tibble::new_tibble(list(class = rep(patches_class, nrow(enn)), value = enn$dist))
    }))
}

#' get_perimeter_patch
#'
#' @description Perimeter of each patch in each class
#'
#' @param classes A vector with unique values (output of get_unique_values_int)
#' @param class_patches A list with matrices of patches for each class (output of get_class_patches)
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#'
#' @details
#' Calculate perimeter of each patch in each class
#'
#' @return A tibble with two columns: class, value
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' classes <- landscapemetrics:::get_unique_values_int(landscape_mat)
#' class_patches <- get_class_patches(landscape_mat, classes, directions = 8)
#' pp <- get_perimeter_patch(classes, class_patches, terra::res(landscape))
#'
#' @keywords internal
#'
#' @export
get_perimeter_patch <- function(classes, class_patches, resolution) {
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    # raster resolution not identical in x-y directions
    if (!isTRUE(all.equal(resolution_x, resolution_y))) {

        top_bottom_matrix <- matrix(c(NA, NA, NA,
                                      1,  0, 1,
                                      NA, NA, NA), 3, 3, byrow = TRUE)

        left_right_matrix <- matrix(c(NA, 1, NA,
                                      NA, 0, NA,
                                      NA, 1, NA), 3, 3, byrow = TRUE)
    }

    perimeter_patch <- do.call(rbind,
                               lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- class_patches[[as.character(patches_class)]]

        # cells at the boundary of the landscape need neighbours to calculate perim
        landscape_labeled <- pad_raster_internal(landscape_labeled,
                                                 pad_raster_value = NA,
                                                 pad_raster_cells = 1,
                                                 global = FALSE)

        # which cells are NA (i.e. background)
        target_na <- which(is.na(landscape_labeled))

        # set all NA to -999 to get adjacencies between patches and all background
        landscape_labeled[target_na] <- -999

        # x-y resolution is identical
        if (isTRUE(all.equal(resolution_x, resolution_y))) {

            # get coocurrence matrix
            neighbour_matrix <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                            directions = as.matrix(4),
                                                            single_class = -999)

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij <- neighbour_matrix[2:nrow(neighbour_matrix), 1] * resolution_x

        # x-y resolution not identical, count adjacencies separately for x- and y-direction
        } else {

            # get coocurrence matrix in x-direction
            left_right_neighbours <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                                 directions = as.matrix(left_right_matrix),
                                                                 single_class = -999)

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij_left_right <- left_right_neighbours[2:nrow(left_right_neighbours), 1] * resolution_x

            # get coocurrennce matrix in y-direction
            top_bottom_neighbours <- rcpp_get_coocurrence_matrix_single(landscape_labeled,
                                                                 directions = as.matrix(top_bottom_matrix),
                                                                 single_class = -999)

            # get adjacencies between patches and background cells (-999 always first row of matrix) and convert to perimeter
            perimeter_patch_ij_top_bottom <- top_bottom_neighbours[2:nrow(top_bottom_neighbours), 1] * resolution_y

            # add perim of both directions for each patch
            perimeter_patch_ij <- perimeter_patch_ij_top_bottom + perimeter_patch_ij_left_right
        }

        tibble::new_tibble(list(class = rep(patches_class, length(perimeter_patch_ij)),
                       value = perimeter_patch_ij))
        })
    )
}
