#' Euclidean Nearest Neighbor Distance Distribution (ENN_MN)
#'
#' @description Euclidean Nearest Neighbor Distance Distribution (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_mn(landscape)
#'
#' @aliases lsm_c_enn_mn
#' @rdname lsm_c_enn_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_enn_mn <- function(landscape) UseMethod("lsm_c_area_mn")

#' @name lsm_c_area_mn
#' @export
lsm_c_enn_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_mn
#' @export
lsm_c_enn_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_enn_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_enn_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_enn_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_mn_calc <- function(landscape) {
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
                                                  tibble(
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
            dist_mat_list[[x]][upper.tri(dist_mat_list[[x]], diag = TRUE)] <- NA
            dist_mat_list[[x]] %>%
                mean(., na.rm = TRUE)
        })

    tibble::tibble(
        level = "class",
        id = seq_along(dist_mat_list_mean),
        metric = "euclidean nearest neighbor distance distribution (mean)",
        value = unlist(dist_mat_list_mean)
    )

}
