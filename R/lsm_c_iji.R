#'Interspersion and Juxtaposition index (class level)
#
#' @description Interspersion and Juxtaposition index (class level)
#
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary ???
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#
#' @details
#' ???
#' \deqn{???}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#
#' @return tibble
#
#' @examples
#'lsm_c_iji(landscape)
#
#' @aliases lsm_c_iji
#' @rdname lsm_c_iji
#
#' @references
#'McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#'Program for Categorical and Continuous Maps. Computer software program produced by
#'the authors at the University of Massachusetts, Amherst. Available at the following
#'web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#
#' @export
lsm_c_iji <- function(landscape) UseMethod("lsm_c_iji")
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterLayer <- function(landscape) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterStack <- function(landscape) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterBrick <- function(landscape) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.list <- function(landscape) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}

lsm_c_iji_calc <- function(landscape) {

    adj <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                       as.matrix(4))

    diag(adj) <- 0

    iji <- purrr::map(seq_len(ncol(adj)), function(row_ind) {
        purrr::map_dbl(seq_len(ncol(adj)), function(col_ind) {
            (adj[row_ind, col_ind] / sum(adj[row_ind, ])) *
                log((adj[row_ind, col_ind] / sum(adj[row_ind, ])))
        })
    })


    iji <- purrr::map_dbl(iji, sum, na.rm = TRUE)

    iji <- -(iji / log(ncol(adj) - 1)) * 100


 tibble::tibble(
     level = "class",
     class = as.integer(raster::unique(landscape)),
     id = as.integer(NA),
     metric = "iji",
     value = as.double(iji)
 )
}
