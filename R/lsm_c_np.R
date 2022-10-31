#' NP (class level)
#'
#' @description Number of patches (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{NP = n_{i}}
#' where \eqn{n_{i}} is the number of patches.
#'
#' NP is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{NP >= 1}
#' \subsection{Behaviour}{Equals NP = 1 when only one patch is present and
#' increases, without limit, as the number of patches increases}
#'
#' @seealso
#' \code{\link{lsm_l_np}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_np(landscape)
#'
#' @aliases lsm_c_np
#' @rdname lsm_c_np
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_c_np <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_np_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_np_calc <- function(landscape, directions){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "np",
                              value = as.double(NA)))
    }

    # get unique classes
    classes <- get_unique_values_int(landscape, verbose = FALSE)

    # get number of patches
    np_class <- lapply(X = classes, FUN = function(patches_class) {

        # connected labeling current class
        landscape_labeled <- get_patches_int(landscape,
                                             class = patches_class,
                                             directions = directions)[[1]]

        # max(patch_id) equals number of patches
        np <- max(landscape_labeled, na.rm = TRUE)

        tibble::tibble(
            level = "class",
            class = as.integer(patches_class),
            id = as.integer(NA),
            metric = "np",
            value = as.double(np))
        })

    do.call(rbind, np_class)
}
