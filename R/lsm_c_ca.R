#
# cclabeled_raster <- cclabel(landscape)
#
# cclabel_points <- purrr::map(seq_along(cclabeled_raster),
#                              function(x) {
#                                  purrr::map(raster::unique(cclabeled_raster[[x]]),
#                                             function(y) {
#                                                 raster::rasterToPoints(
#                                                     cclabeled_raster[[x]],
#                                                     fun = function(z) {
#                                                         z == y
#                                                     }
#                                                 )
#                                             })
#                              })
#
#
# cclabel_points_dist <- purrr::map(seq_along(cclabel_points),
#                                   function(x) {
#                                       purrr::map(seq_along(cclabel_points[[x]]), function(y) {
#                                           purrr::map(seq_along(cclabel_points[[x]]), function(z) {
#                                               x <- raster::pointDistance(cclabel_points[[x]][[y]][, 1:2],
#                                                                          cclabel_points[[x]][[z]][, 1:2],
#                                                                          lonlat = FALSE)
#                                               tibble::as.tibble(x)
#                                           })
#                                       })
#                                   })
#
#
#
# cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_dist),function(x) {
#     purrr::map(seq_along(cclabel_points_dist[[x]]), function(y) {
#         purrr::map(seq_along(cclabel_points_dist[[x]][[y]]), function(z) {
#
#             sum(cclabel_points_dist[[x]][[y]][[z]] > prox_radius) / min(cclabel_points_dist[[x]][[y]][[z]])
#
#         })
#     })
# })
#
# cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_prox),function(x) {
#     purrr::map(seq_along(cclabel_points_prox[[x]]), function(y) {
#         sum(unlist(cclabel_points_prox[[x]][[y]]), na.rm = T)
#     })
# })
#
#
# tibble::tibble(
#     level = "patch",
#     class = unlist(purrr::map(seq_along(cclabel_points_prox), function(x){
#         rep(x, length(cclabel_points_prox[[x]]))
#     })),
#     id = seq_len(length(unlist(cclabel_points_prox))),
#     metric = "proximity index",
#     value = unlist(cclabel_points_prox)
# ) %>% View()
#
# cclabeled_raster <- cclabel(landscape)
#
# cclabel_points <- purrr::map(seq_along(cclabeled_raster),
#                              function(x) {
#                                  purrr::map(raster::unique(cclabeled_raster[[x]]),
#                                             function(y) {
#                                                 raster::rasterToPoints(
#                                                     cclabeled_raster[[x]],
#                                                     fun = function(z) {
#                                                         z == y
#                                                     }
#                                                 )
#                                             })
#                              })
#
#
# cclabel_points_dist <- purrr::map(seq_along(cclabel_points),
#                                   function(x) {
#                                       purrr::map(seq_along(cclabel_points[[x]]), function(y) {
#                                           purrr::map(seq_along(cclabel_points[[x]]), function(z) {
#                                               x <- raster::pointDistance(cclabel_points[[x]][[y]][, 1:2],
#                                                                          cclabel_points[[x]][[z]][, 1:2],
#                                                                          lonlat = FALSE)
#                                               tibble::as.tibble(x)
#                                           })
#                                       })
#                                   })
#
#
#
# cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_dist),function(x) {
#     purrr::map(seq_along(cclabel_points_dist[[x]]), function(y) {
#         purrr::map(seq_along(cclabel_points_dist[[x]][[y]]), function(z) {
#
#             sum(cclabel_points_dist[[x]][[y]][[z]] > prox_radius) / min(cclabel_points_dist[[x]][[y]][[z]])
#
#         })
#     })
# })
#
# cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_prox),function(x) {
#     purrr::map(seq_along(cclabel_points_prox[[x]]), function(y) {
#         sum(unlist(cclabel_points_prox[[x]][[y]]), na.rm = T)
#     })
# })
#
#
# tibble::tibble(
#     level = "patch",
#     class = unlist(purrr::map(seq_along(cclabel_points_prox), function(x){
#         rep(x, length(cclabel_points_prox[[x]]))
#     })),
#     id = seq_len(length(unlist(cclabel_points_prox))),
#     metric = "proximity index",
#     value = unlist(cclabel_points_prox)
# ) %>% View()
#' Total class area (class level)
#'
#' @description Total area of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Total class area equals the sum of the area of all patches of class i. Total class area is
#' an absolute measure, making comparisons among landscapes with different
#' total areas difficult. It is a measure of landscape composition.
#' \deqn{CA = sum(area[patch_i])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{CA increases without limit as the amount of the class increases.
#' CA = TA if only one class is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ca(landscape)
#'
#' @aliases lsm_c_ca
#' @rdname lsm_c_ca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_ca <- function(landscape) UseMethod("lsm_c_ca")

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ca_calc <- function(landscape) {
    total_area <- landscape %>%
        lsm_p_area() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = total_area$class,
        id = as.integer(NA),
        metric = "total area",
        value = total_area$value
    )
}
