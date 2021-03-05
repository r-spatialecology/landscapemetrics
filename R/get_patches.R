#' get_patches
#'
#' @description Connected components labeling to derive patches in a landscape.
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' # check for patches of class 1
#' patched_raster <- get_patches(landscape, class = 1)
#'
#' # count patches
#' length(raster::unique(patched_raster[[1]]))
#'
#' # check for patches of every class
#' patched_raster <-  get_patches(landscape)
#'
#' @aliases get_patches
#' @rdname get_patches
#'
#' @export
get_patches <- function(landscape,
                        class,
                        directions,
                        to_disk,
                        return_raster)  UseMethod("get_patches")


#' @name get_patches
#' @export
get_patches.RasterLayer <- function(landscape,
                                    class = "all",
                                    directions = 8,
                                    to_disk = getOption("to_disk", default = FALSE),
                                    return_raster = TRUE) {

    # convert landscape to matrix
    landscape_matrix <- raster::as.matrix(landscape)

    # get connected components
    result <- get_patches_int(landscape = landscape_matrix,
                              class = class,
                              directions = directions)

    # convert back to raster
    if (return_raster) {

        result <- lapply(result,
                         FUN = matrix_to_raster,
                         landscape = landscape,
                         to_disk = to_disk)
    }

    return(result)

}

#' @name get_patches
#' @export
get_patches.RasterStack <- function(landscape,
                                    class = "all",
                                    directions = 8,
                                    to_disk = getOption("to_disk", default = FALSE),
                                    return_raster = TRUE) {

    result <- lapply(X = raster::as.list(landscape),

                     FUN = function(x, class, directions, return_raster, to_disk) {

                         x_matrix <- raster::as.matrix(x)

                         result <- get_patches_int(x_matrix, class, directions)

                         if (return_raster) {

                             result <- lapply(result,
                                              FUN = matrix_to_raster,
                                              landscape = x,
                                              to_disk = to_disk)
                         }

                         return(result)
                     },

                     class = class,
                     directions = directions,
                     return_raster = return_raster,
                     to_disk = to_disk)

    return(result)
}

#' @name get_patches
#' @export
get_patches.RasterBrick <- function(landscape,
                                    class = "all",
                                    directions = 8,
                                    to_disk = getOption("to_disk", default = FALSE),
                                    return_raster = TRUE) {

    result <- lapply(X = raster::as.list(landscape),

                     FUN = function(x, class, directions, return_raster, to_disk) {

                         x_matrix <- raster::as.matrix(x)

                         result <- get_patches_int(x_matrix, class, directions)

                         if (return_raster) {

                             result <- lapply(result,
                                              FUN = matrix_to_raster,
                                              landscape = x,
                                              to_disk = to_disk)
                         }

                         return(result)
                     },

                     class = class,
                     directions = directions,
                     return_raster = return_raster,
                     to_disk = to_disk)

    return(result)
}

#' @name get_patches
#' @export
get_patches.stars <- function(landscape,
                              class = "all",
                              directions = 8,
                              to_disk = getOption("to_disk", default = FALSE),
                              return_raster = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),

                     FUN = function(x, class, directions, return_raster, to_disk) {

                         x_matrix <- raster::as.matrix(x)

                         result <- get_patches_int(x_matrix, class, directions)

                         if (return_raster) {
                             result <- lapply(result,
                                              FUN = matrix_to_raster,
                                              landscape = x,
                                              to_disk = to_disk)
                         }

                         return(result)
                     },

                     class = class,
                     directions = directions,
                     return_raster = return_raster,
                     to_disk = to_disk)

    return(result)
}

#' @name get_patches
#' @export
get_patches.list <- function(landscape,
                             class = "all",
                             directions = 8,
                             to_disk = getOption("to_disk", default = FALSE),
                             return_raster = TRUE) {

    result <- lapply(X = raster::as.list(landscape),

                     FUN = function(x, class, directions, return_raster, to_disk) {

                         if (inherits(x = x, what = "RasterLayer")) {

                             x_matrix <- raster::as.matrix(x)

                             result <- get_patches_int(x_matrix, class, directions)

                             if (return_raster) {

                                 result <- lapply(result,
                                                  FUN = matrix_to_raster,
                                                  landscape = x,
                                                  to_disk = to_disk)
                             }
                             return(result)
                         }

                         else {
                             result <- get_patches_int(x, class, directions)
                             return(result)
                         }

                     },

                     class = class,
                     directions = directions,
                     return_raster = return_raster,
                     to_disk = to_disk)

    return(result)
}

#' @name get_patches
#' @export
get_patches.matrix <- function(landscape,
                               class = "all",
                               directions = 8,
                               to_disk = getOption("to_disk", default = FALSE),
                               return_raster = FALSE) {

    result <- get_patches_int(landscape,
                              class = class,
                              directions = directions)

    if (return_raster || to_disk) {
        warning("'return_raster = TRUE' or 'to_disk = TRUE' not able for matrix input.",
                call. = FALSE)
    }

    return(result)
}

get_patches_int <- function(landscape,
                            class,
                            directions) {

    # check if directions argument is valid
    if (directions != 4 && directions != 8) {

        warning("You must specify a directions parameter. Defaulted to 8.",
                call. = FALSE)

        directions <- 8
    }

    # get unique classes
    unique_classes <- get_unique_values(landscape)[[1]]

    # set-up filter matrix
    filter_matrix <- matrix(NA,
                            nrow = nrow(landscape),
                            ncol = ncol(landscape))

    # class is specified
    if (any(class != "all")) {

        # check if class is present in landscape
        if (!any(class %in% unique_classes)) {
            stop("Not all provided classes present in landscape")
        }

        patch_landscape <- lapply(X = class, FUN = function(current_class) {

            # set all values in filter_matrix to 1 that belong to class (at same spot as in original landscape)
            filter_matrix[landscape == current_class] <- 1L

            # connected labeling with 4 or 8 neighbours
            rcpp_ccl(filter_matrix, directions)
            patch_landscape <- filter_matrix

            return(patch_landscape)
        })

        names(patch_landscape) <- class
        return(patch_landscape)
    }

    else {

        patch_landscape <- lapply(X = unique_classes, FUN = function(class) {

            filter_matrix[landscape == class] <- 1L

            # connected labeling with 4 or 8 neighbours
            rcpp_ccl(filter_matrix, directions)
            patch_landscape <- filter_matrix

            return(patch_landscape)
        })

        names(patch_landscape) <- unique_classes

        patch_landscape <- patch_landscape[order(as.integer(names(patch_landscape)))]

        return(patch_landscape)
    }
}
