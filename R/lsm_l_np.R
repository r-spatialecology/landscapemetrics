#' NP (landscape level)
#'
#' @description Number of patches (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{NP = N}
#' where \eqn{N} is the number of patches.
#'
#' NP is an 'Aggregation metric'. It describes the fragmentation of the landscape,
#' however, does not necessarily contain information about the configuration or
#' composition of the landscape.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{NP >= 1}
#' \subsection{Behaviour}{Equals NP = 1 when only one patch is present and
#' increases, without limit, as the number of patches increases}
#'
#' @seealso
#' \code{\link{lsm_c_np}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_np(landscape)
#'
#' @aliases lsm_l_np
#' @rdname lsm_l_np
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_np <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_np_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_np_calc <- function(landscape, directions) {

    n_patches <- lsm_c_np_calc(landscape,
                               directions = directions)

    n_patches <- sum(n_patches$value)

    # all values NA
    if (is.na(n_patches)) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "np",
                              value = as.double(NA)))
    }

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "np",
                          value = as.double(n_patches)))
}
