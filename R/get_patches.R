#' get_patches
#'
#' @description Connected components labeling to derive patches in a landscape.
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param class Either "all" (default) for every class in the raster, or specify
#' class value. See Details.
#' @param to_disk Logical argument, if FALSE results of get_patches are hold
#' in memory. If true, get_patches writes temporary files and hence, does not hold everything in memory.
#' Can be set with a global option, e.g. `option(to_disk = TRUE)`. See Details.
#' @param return_raster If false, matrix is returned
#'
#' @details
#' Searches for connected patches (neighbouring cells of the same class i).
#' The 8-neighbours rule ('queen's case) or 4-neighbours rule (rook's case) is
#' used. Returns a list with raster. For each class the connected patches have
#' the value 1 - n. All cells not belonging to the class are \code{NA}.
#'
#' Landscape metrics rely on the delineation of patches. Hence, `get_patches` is
#' heavily used in **landscapemetrics**. As raster can be quite big, the fact that
#' `get_patches` creates a copy of the raster for each class in a landscape becomes
#' a burden for computer memory. Hence, the argument *to_disk* allows to
#' store the results of the connected labeling algorithm on disk. Furthermore,
#' this option can be set globally, so that every function that internally uses
#' `get_patches` can make use of that.
#'
#' @references
#' Vincent, L., Soille, P. 1991. Watersheds in digital spaces: an efficient
#' algorithm based on immersion simulations. IEEE Transactions on Pattern
#' Analysis and Machine Intelligence. 13 (6), 583-598
#'
#' @return List
#'
#' @examples
#' landscape <- terra::unwrap(landscape)
#'
#' # check for patches of class 1
#' patched_raster <- get_patches(landscape, class = 1)
#'
#' # count patches
#' nrow(terra::unique(patched_raster[[1]][[1]]))
#'
#' # check for patches of every class
#' patched_raster <-  get_patches(landscape)
#'
#' @aliases get_patches
#' @rdname get_patches
#'
#' @export
get_patches <- function(landscape, class = "all", directions = 8,
                        to_disk = getOption("to_disk", default = FALSE),
                        return_raster = TRUE) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape, FUN = function(i) {

        res_temp <- get_patches_int(landscape = i, class = "all", directions = directions,
                                    return_raster = return_raster, to_disk = to_disk)

        # filter returned classes
        if (all(class != "all")) {

            res_temp <- res_temp[names(res_temp) == paste0("class_", class)]

            if (length(res_temp) == 0) {

                stop("Selected class not present in landscape.", call. = FALSE)

            }

        }

        return(res_temp)
    })

    names(result) <- paste0("layer_", 1:length(landscape))

    return(result)

}

get_patches_int <- function(landscape, class, directions,
                            return_raster = FALSE, to_disk = FALSE) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        landscape_mat <- terra::as.matrix(landscape, wide = TRUE)

    # already a matrix
    } else {

        landscape_mat <- landscape

        if (return_raster || to_disk) {

            return_raster <- FALSE

            warning("'return_raster' or 'to_disk' not possible for matrix input.", call. = FALSE)

        }
    }

    # check if directions argument is valid
    if (directions != 4 && directions != 8) {

        warning("You must specify a directions parameter. Defaulted to 8.",
                call. = FALSE)

        directions <- 8
    }

    if (class == "all") {

        # get unique class id
        unique_classes <- get_unique_values_int(landscape_mat, verbose = FALSE)

    } else {

        unique_classes <- class

    }

    # init highest patch id
    counter_id <- 0

    # init list with classes
    patch_landscape <- vector(mode = "list", length = length(unique_classes))

    # CCL all classes
    for (i in 1:length(unique_classes)) {

        # set-up filter matrix
        landscape_temp <- matrix(NA, nrow = nrow(landscape_mat),
                                 ncol = ncol(landscape_mat))

        # set all values in filter_matrix to 1 that belong to class (at same spot as in original landscape)
        landscape_temp[landscape_mat == unique_classes[i]] <- 1L

        # connected labeling with 4 neighbours
        if (directions == 4) {

            rcpp_ccl(landscape_temp, 4)

        # connected labeling with 8 neighbours
        } else if (directions == 8) {

            rcpp_ccl(landscape_temp, 8)

        }

        # increase patch id by highest value so far
        landscape_temp <- landscape_temp + counter_id

        # update highest patch id
        counter_id <- max(landscape_temp, na.rm = TRUE)

        # return matrix to
        if (return_raster) {

            landscape_temp <- matrix_to_raster(matrix = landscape_temp,
                                                landscape = landscape, to_disk = to_disk)
        }

        patch_landscape[[i]] <- landscape_temp

    }

    # set class names
    names(patch_landscape) <- paste0("class_", unique_classes)

    return(patch_landscape)
}
