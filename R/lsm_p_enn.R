#' ENN (patch level)
#'
#' @description Euclidean Nearest-Neighbor Distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{ENN = h_{ij}}
#' where \eqn{h_{ij}} is the distance to the nearest neighbouring patch of
#' the same class i in meters
#'
#' ENN is an 'Aggregation metric'. The distance to the nearest neighbouring patch of
#' the same class i. The distance is measured from edge-to-edge. The range is limited by the
#' cell resolution on the lower limit and the landscape extent on the upper limit. The metric
#' is a simple way to describe patch isolation.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN > 0}
#' \subsection{Behaviour}{Approaches ENN = 0 as the distance to the nearest neighbour
#' decreases, i.e. patches of the same class i are more aggregated. Increases, without limit,
#' as the distance between neighbouring patches of the same class i increases, i.e. patches are
#' more isolated.}
#'
#' @seealso
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_enn(landscape)
#'
#' @aliases lsm_p_enn
#' @rdname lsm_p_enn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' McGarigal, K., and McComb, W. C. (1995). Relationships between landscape
#' structure and breeding birds in the Oregon Coast Range.
#' Ecological monographs, 65(3), 235-260.
#'
#' @export
lsm_p_enn <- function(landscape, directions, verbose) UseMethod("lsm_p_enn")

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterLayer <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_enn_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterStack <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_enn_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterBrick <- function(landscape, directions = 8, verbose = TRUE) {

     result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_enn_calc,
                     directions = directions,
                     verbose = verbose)

     layer <- rep(seq_along(result),
                  vapply(result, nrow, FUN.VALUE = integer(1)))

     result <- do.call(rbind, result)

     tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.stars <- function(landscape, directions = 8, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_enn_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.list <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_enn_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_enn_calc <- function(landscape, directions, verbose,
                           points = NULL) {

    # conver to matrix
    if (class(landscape) != "matrix") {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # convert to matrix
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "enn",
                              value = as.double(NA)))
    }

    # get unique classes
    classes <- get_unique_values(landscape)[[1]]

    enn_patch <- do.call(rbind,
                         lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions,
                                         return_raster = FALSE)[[1]]

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
        }

        else {

            # get edge cells because only they are important for ENN
            class_boundaries <- get_boundaries(landscape_labeled,
                                               directions = 4,
                                               as_NA = TRUE)[[1]]

            # set edge cell value to patch id
            class_boundaries[!is.na(class_boundaries)] <- landscape_labeled[!is.na(class_boundaries)]

            # transpose to get same direction of ID
            class_boundaries <- t(class_boundaries)

            # get coordinates of current class
            points <- points[which(!is.na(class_boundaries)), ]

            # set ID from class ID to unique patch ID
            points[, 3] <- class_boundaries[!is.na(class_boundaries)]

            # order points
            ord <- order(as.matrix(points)[, 1])
            num <- seq_along(ord)
            rank <- match(num, ord)

            # get nearest neighbor between patches
            res <- rcpp_get_nearest_neighbor(as.matrix(points)[ord,])

            # order results
            min_dist <- unname(cbind(num, res[rank], as.matrix(points)[, 3]))

            tbl <- tibble::tibble(cell = min_dist[,1],
                                  dist = min_dist[,2],
                                  id = min_dist[,3])

            # only get minimum value for each patch
            enn <- stats::aggregate(x = tbl[, 2], by = tbl[, 3], FUN = min)
        }

        tibble::tibble(class = patches_class,
                       value = enn$dist)
        })
    )

    tibble::tibble(level = "patch",
                   class = as.integer(enn_patch$class),
                   id = as.integer(seq_len(nrow(enn_patch))),
                   metric = "enn",
                   value = as.double(enn_patch$value))
}
