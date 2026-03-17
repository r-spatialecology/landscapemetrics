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
        class_name <- paste0("class_", patches_class)
        landscape_labeled <- get_patches_int(
            landscape_mat,
            class = patches_class,
            directions = directions
        )[[class_name]]
        landscape_labeled
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
    # flatten list of named vectors to single named vector
    area_patches <- do.call(c,
                            lapply(classes, function(patches_class) {
        landscape_labeled <- class_patches[[as.character(patches_class)]]
        area_patch_ij <- rcpp_get_composition_vector(x = landscape_labeled) * factor_ha
        stats::setNames(area_patch_ij, rep(as.character(patches_class), length(area_patch_ij)))
    }))
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
    points <- expand.grid(
        col = seq_len(ncol(landscape_mat)),
        row = seq_len(nrow(landscape_mat))
    )
    points$col <- points$col * resolution[[1]]
    points$row <- points$row * resolution[[2]]
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
#' @return A named numeric vector of ENN values. Names correspond to class IDs
#' (repeated for each patch within a class).
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
    enn_patch <- do.call(c, lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- class_patches[[as.character(patches_class)]]

        # get number of patches
        np_class <- max(landscape_labeled, na.rm = TRUE)

        # ENN doesn't make sense if only one patch is present
        if (np_class == 1) {

            enn_dist <- as.double(NA)

            if (verbose) {
                warning(paste0("Class ", patches_class, ": ENN = NA for class with only 1 patch."),
                        call. = FALSE)
            }
        } else {

            enn <- get_nearestneighbour_calc(landscape = landscape_labeled, return_id = FALSE,
                                             resolution = resolution,
                                             points = points)
            enn_dist <- enn$dist
        }

        stats::setNames(enn_dist, rep(as.character(patches_class), length(enn_dist)))
    }))

    enn_patch
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
#' @return A named vector with perimeter values for each patch (names are class IDs)
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

    perimeter_patch <- do.call(c,
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

        stats::setNames(perimeter_patch_ij, rep(as.character(patches_class), length(perimeter_patch_ij)))
        })
    )

    perimeter_patch
}

#' get_core_patch
#'
#' @description Core area of each patch in each class
#'
#' @param landscape_mat A matrix object
#' @param classes A vector with unique values (output of get_unique_values_int)
#' @param class_patches A list with matrices of patches for each class (output of get_class_patches)
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has to be away from the patch edge to be considered as core cell
#' @param resolution A vector with two numbers (usually calculated using terra::res)
#'
#' @details
#' Calculate core area of each patch in each class. Core area is the area within a patch
#' that is not on the edge. A cell is defined as core area if the cell has no neighbour
#' with a different value than itself (rook's case).
#'
#' @return A named vector with core area values for each patch (names are class IDs)
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' landscape_mat <- terra::as.matrix(landscape, wide = TRUE)
#' classes <- landscapemetrics:::get_unique_values_int(landscape_mat)
#' class_patches <- get_class_patches(landscape_mat, classes, directions = 8)
#' core <- get_core_patch(landscape_mat, classes, class_patches, 8, FALSE, 1, terra::res(landscape))
#'
#' @keywords internal
#'
#' @export
get_core_patch <- function(landscape_mat, classes, class_patches, directions,
                           consider_boundary, edge_depth, resolution) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    core <- do.call(c,
                    lapply(classes, function(patches_class) {

                        # get connected patches
                        landscape_labeled <- class_patches[[as.character(patches_class)]]

                        # get existing patch IDs (non-NA values)
                        patch_ids <- sort(unique(as.vector(landscape_labeled[!is.na(landscape_labeled)])))

                        # label all edge cells
                        class_edge <- get_boundaries_calc(landscape_labeled,
                                                          edge_depth = edge_depth,
                                                          consider_boundary = consider_boundary,
                                                          as_NA = FALSE,
                                                          patch_id = FALSE)

                        # count number of edge cells in each patch (edge == 1)
                        cells_edge_patch <- tabulate(landscape_labeled[class_edge == 1])

                        # all cells of the patch
                        cells_patch <- tabulate(landscape_labeled)

                        # only keep values for existing patches
                        cells_edge_patch <- cells_edge_patch[patch_ids]
                        cells_patch <- cells_patch[patch_ids]

                        # check if no cell is edge, i.e. only one patch is present
                        if (length(cells_edge_patch) == 0) {
                            cells_edge_patch <- 0
                        }

                        # all cells minus edge cells equal core and convert to ha
                        core_area <- (cells_patch - cells_edge_patch) * prod(resolution) / 10000

                        # return named vector: names are class IDs, values are core areas
                        stats::setNames(core_area, rep(as.character(patches_class), length(core_area)))
                    })
    )

    # return named vector (preserve names)
    structure(as.double(core), names = names(core))
}

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
#' @param consider_boundary Logical if cells that only neighbour the landscape boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has to be away from the patch edge to be considered as core cell
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
#'  \item enn_patch: named numeric vector with euclidean nearest neighbour distances
#'  \item core_patch: vector with core area for each patch
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
prepare_extras <- function(metrics, landscape_mat, directions, neighbourhood, ordered, base, resolution,
                           consider_boundary = FALSE, edge_depth = 1){
    required <- unique(extras_df$extras[extras_df$metric %in% metrics])

    resolve_extras(
        landscape_mat = landscape_mat,
        directions = directions,
        required = required,
        neighbourhood = neighbourhood,
        ordered = ordered,
        base = base,
        resolution = resolution,
        consider_boundary = consider_boundary,
        edge_depth = edge_depth
    )
}

resolve_extras <- function(landscape_mat, directions = NULL, extras = NULL, required = character(),
                           neighbourhood = NULL, ordered = NULL, base = NULL, resolution = NULL,
                           consider_boundary = NULL, edge_depth = NULL) {
    if (!inherits(x = landscape_mat, what = "matrix")) {
        stop("'landscape_mat' must be a matrix.", call. = FALSE)
    }

    required <- unique(required)
    valid_required <- unique(extras_df$extras)

    unknown_required <- setdiff(required, valid_required)
    if (length(unknown_required) > 0) {
        stop("Unknown dependency requested: ", paste(unknown_required, collapse = ", "),
             call. = FALSE)
    }

    if (is.null(extras)) {
        extras <- list()
    }

    params <- list(
        directions = directions,
        neighbourhood = neighbourhood,
        ordered = ordered,
        base = base,
        resolution = resolution,
        consider_boundary = consider_boundary,
        edge_depth = edge_depth
    )

    require_arg <- function(arg_name, extra_name) {
        if (is.null(params[[arg_name]])) {
            stop("'", arg_name, "' is required to resolve '", extra_name, "'.",
                 call. = FALSE)
        }
        invisible(NULL)
    }

    deps <- list(
        points = character(),
        classes = character(),
        class_patches = "classes",
        area_patches = c("classes", "class_patches"),
        composition_vector = character(),
        neighbor_matrix = character(),
        comp = "neighbor_matrix",
        cplx = character(),
        enn_patch = c("classes", "class_patches", "points"),
        perimeter_patch = c("classes", "class_patches"),
        core_patch = c("classes", "class_patches")
    )

    required_args <- list(
        points = "resolution",
        class_patches = "directions",
        area_patches = "resolution",
        neighbor_matrix = "neighbourhood",
        comp = "base",
        cplx = c("neighbourhood", "ordered", "base"),
        enn_patch = "resolution",
        perimeter_patch = "resolution",
        core_patch = c("directions", "consider_boundary", "edge_depth", "resolution")
    )

    compute <- list(
        points = function(ex) get_points(landscape_mat, params$resolution),
        classes = function(ex) get_unique_values_int(landscape_mat, verbose = FALSE),
        class_patches = function(ex) get_class_patches(landscape_mat, ex$classes, params$directions),
        area_patches = function(ex) get_area_patches(ex$class_patches, ex$classes, params$resolution),
        composition_vector = function(ex) rcpp_get_composition_vector(landscape_mat),
        neighbor_matrix = function(ex) {
            rcpp_get_coocurrence_matrix(landscape_mat, directions = as.matrix(params$neighbourhood))
        },
        comp = function(ex) rcpp_get_entropy(colSums(ex$neighbor_matrix), params$base),
        cplx = function(ex) get_complexity(
            landscape_mat,
            params$neighbourhood,
            params$ordered,
            params$base
        ),
        enn_patch = function(ex) get_enn_patch(
            ex$classes, ex$class_patches, ex$points, params$resolution
        ),
        perimeter_patch = function(ex) get_perimeter_patch(
            ex$classes, ex$class_patches, params$resolution
        ),
        core_patch = function(ex) get_core_patch(
            landscape_mat,
            ex$classes,
            ex$class_patches,
            params$directions,
            params$consider_boundary,
            params$edge_depth,
            params$resolution
        )
    )

    map_keys <- names(deps)
    if (!setequal(map_keys, names(compute)) || !setequal(map_keys, names(required_args))) {
        stop("Internal resolver maps are inconsistent.", call. = FALSE)
    }

    resolve_one <- function(name, ex) {
        if (!is.null(ex[[name]])) {
            return(ex)
        }

        for (arg_name in required_args[[name]]) {
            require_arg(arg_name, name)
        }

        for (dep_name in deps[[name]]) {
            ex <- resolve_one(dep_name, ex)
        }

        ex[[name]] <- compute[[name]](ex)
        ex
    }

    for (extra_name in required) {
        extras <- resolve_one(extra_name, extras)
    }

    extras
}
