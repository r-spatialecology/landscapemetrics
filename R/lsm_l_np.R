#' NP (landscape level)
#'
#' @description Number of patches (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
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
lsm_l_np <- function(landscape, directions) UseMethod("lsm_l_np")

#' @name lsm_l_np
#' @export
lsm_l_np.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_np_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        directions = directions,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        directions = directions,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.list <- function(landscape, directions = 8) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        directions = directions,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_np_calc <- function(landscape, directions) {

    n_patches <- landscape %>%
        lsm_c_np_calc(directions = directions) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "np",
        value = as.double(n_patches$value)
    )

}
