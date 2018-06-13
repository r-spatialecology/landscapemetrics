#' Related Circumscribing Circle (CIRCLE)
#'
#' @description Related Circumscribing Circle (patch)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the 1 - the patch area (m^2) divided by the area (m^2) of the smallest
#' circumscribing circle.
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= CIRCLE < 1}
#' \subsection{Behaviour}{CIRCLE = 0 for circular patches and approaches 1 for linear patches}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_circle(landscape)
#'
#' @aliases lsm_p_circle
#' @rdname lsm_p_circle
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_circle <- function(landscape) UseMethod("lsm_p_circle")

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_circle_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_circle_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_circle_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_circle
#' @export
lsm_p_circle.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_circle_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_circle_calc <- function(landscape) {
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




    cclabel_patch_celledges <- purrr::map(seq_along(cclabel_points),
                                          function(class) {
                                              purrr::map(seq_along(cclabel_points[[class]]), function(patch) {
                                                  purrr::map_dfr(seq_len(nrow(cclabel_points[[class]][[patch]])), function(cell) {
                                                      xy1 <-
                                                          cclabel_points[[class]][[patch]][cell, 1:2] - raster::res(landscape) / 2


                                                      xy2 <-
                                                          c(
                                                              cclabel_points[[class]][[patch]][cell, 1] - raster::res(landscape)[1] / 2,
                                                              cclabel_points[[class]][[patch]][cell, 2] + raster::res(landscape)[1] / 2
                                                          )
                                                      xy3 <-
                                                          cclabel_points[[class]][[patch]][cell, 1:2] + raster::res(landscape) / 2
                                                      xy4 <-
                                                          c(
                                                              cclabel_points[[class]][[patch]][cell, 1] + raster::res(landscape)[1] / 2,
                                                              cclabel_points[[class]][[patch]][cell, 2] - raster::res(landscape)[1] / 2
                                                          )


                                                      edge_coords <-
                                                          dplyr::bind_rows(xy1, xy2, xy3, xy4)


                                                  })
                                              })
                                          })




    patch_diameter <- purrr::map(seq_along(cclabel_patch_celledges),
                                 function(class) {
                                     purrr::map_dfr(seq_along(cclabel_patch_celledges[[class]]),
                                                    function(patch) {
                                                        tibble::tibble(x = raster::pointDistance(cclabel_patch_celledges[[class]][[patch]],
                                                                                                 lonlat = FALSE) %>%
                                                                           max())


                                                    })
                                 })


    patch_circles <- purrr::map(seq_along(cclabel_patch_celledges),
                                function(class) {
                                    (patch_diameter[[class]][[1]] / 2) ^ 2  * pi


                                })

    patch_circles <- 1 - ((lsm_p_area_calc(landscape)$value * 10000) / unlist(patch_circles))


    tibble::tibble(
        level = "patch",
        class = unlist(purrr::map(seq_along(patch_diameter), function(x) {
            rep(x, length(patch_diameter[[x]][[1]]))
        })),
        id = seq_len(length(patch_circles)),
        metric = "related circumscribing circle",
        value = patch_circles
    )
}

