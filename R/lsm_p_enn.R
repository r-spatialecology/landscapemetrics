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

    patches_class <- lsm_c_np_calc(landscape)

    if(any(patches_class$value == 1)){
        warning("ENN = NA for class with only 1 patch")
    }

    landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(patches_class) {

            class <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            points_class <- patches_class %>%
                raster::rasterToPoints() %>%
                tibble::as.tibble() %>%
                setNames(c("x", "y", "id"))

            np_class <- points_class %>%
                dplyr::pull(id) %>%
                unique() %>%
                sort()

            if(length(np_class) == 1){
                enn <- NA
            }

            else{
                enn <- np_class %>%
                    purrr::map_dbl(function(patch_ij){
                        patch_focal <- points_class %>%
                            dplyr::filter(id == patch_ij)

                        patch_others <- points_class %>%
                            dplyr::filter(id != patch_ij)

                        minimum_distance <- raster::pointDistance(patch_focal[1:2],
                                                                  patch_others[1:2],
                                                                  lonlat = FALSE) %>%
                            min()
                    })
            }

            tibble::tibble(level = "patch",
                           class = as.integer(class),
                           id = as.integer(patch_ij),
                           metric = "euclidean nearest neighbor distance distribution (mean)",
                           value = as.double(enn))
        })
}

