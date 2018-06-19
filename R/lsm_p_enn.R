#' ENN (patch level)
#'
#' @description Euclidean Nearest-Neighbor Distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' @return tibble
#'
#' @examples
#' lsm_p_enn(landscape)
#'
#' @aliases lsm_p_enn
#' @rdname lsm_p_enn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_enn <- function(landscape) UseMethod("lsm_p_enn")

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_enn
#' @export
lsm_p_enn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_enn_calc <- function(landscape) {
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


    cclabel_points_dist <- purrr::map(seq_along(cclabel_points),
                                      function(x) {
                                          purrr::map_dfr(seq_along(cclabel_points[[x]]), function(y) {
                                              purrr::map_dfr(seq_along(cclabel_points[[x]]), function(z) {
                                                  tibble::tibble(
                                                      x = raster::pointDistance(
                                                          cclabel_points[[x]][[y]][, 1:2],
                                                          cclabel_points[[x]][[z]][, 1:2],
                                                          lonlat = FALSE
                                                      ) %>%
                                                          min()
                                                  )
                                              }, .id = "id1")
                                          }, .id = "id2")
                                      })

    dist_mat_list <-
        purrr::map(seq_along(cclabel_points_dist), function(x) {
            matrix(cclabel_points_dist[[x]]$x,
                   max(as.integer(cclabel_points_dist[[x]]$id2)),
                   max(as.integer(cclabel_points_dist[[x]]$id1)))

        })

    dist_mat_list_mean <-
        purrr::map(seq_along(dist_mat_list), function(x) {
            dist_mat_list[[x]][dist_mat_list[[x]] == 0] <- NA
            apply(dist_mat_list[[x]],2,min, na.rm = TRUE)
        })

    tibble::tibble(
        level = "patch",
        class = unlist(purrr::map(seq_along(dist_mat_list_mean), function(x){
            rep(x, length(dist_mat_list_mean[[x]]))
        })),
        id = seq_len(length(unlist(dist_mat_list_mean))),
        metric = "euclidean nearest neighbor distance distribution (mean)",
        value = unlist(dist_mat_list_mean)
    )

}
