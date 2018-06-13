#' Radius of Gyration (patch level)
#'
#' @description Radius of Gyration (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean distance of each cell centroid in a patch to the centroid
#' of the whole patch (mean location of all cell centroids)
#' \deqn{GYRATE = distance to patch centroid[cell_i] / number of cells in patch}
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0, without limit}
#' \subsection{Behaviour}{0 if single cell, maximum if patch occupies the entire landscape}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_gyrate(landscape)
#'
#' @aliases lsm_p_gyrate
#' @rdname lsm_p_gyrate
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_gyrate <- function(landscape) UseMethod("lsm_p_gyrate")

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_gyrate_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_gyrate_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_gyrate_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_gyrate_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_gyrate_calc <- function(landscape) {
    cclabeled_raster <- cclabel(landscape)

    cclabel_points <- purrr::map(seq_along(cclabeled_raster),
                                 function(x) {
                                     purrr::map(raster::unique(cclabeled_raster[[x]]),
                                                function(y) {
                                                    raster::rasterToPoints(
                                                        cclabeled_raster[[x]],
                                                        fun = function(z) {
                                                            z == y
                                                        }
                                                    )
                                                })
                                 })




    cclabel_gyration <- purrr::map(seq_along(cclabel_points),
                                          function(class) {
                                              purrr::map(seq_along(cclabel_points[[class]]), function(patch) {

                                                  mx <- mean(cclabel_points[[class]][[patch]][,1])
                                                  my <- mean(cclabel_points[[class]][[patch]][,2])

                                                  mean(raster::pointDistance(cclabel_points[[class]][[patch]][,1:2],
                                                                        matrix(c(mx,my), ncol = 2, nrow = 1),
                                                                        lonlat = FALSE)) / nrow(cclabel_points[[class]][[patch]])

                                              })
                                          })

    tibble::tibble(
        level = "patch",
        class = unlist(purrr::map(seq_along(cclabel_gyration), function(x) {
            rep(x, length(cclabel_gyration[[x]]))
        })),
        id = seq_len(length(unlist(cclabel_gyration))),
        metric = "radius of gyration",
        value = unlist(cclabel_gyration)
    )
}

