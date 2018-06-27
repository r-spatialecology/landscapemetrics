#' GYRATE (patch level)
#'
#' @description Radius of Gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{GYRATE = \sum_{r=1}^{z} \frac{h_{ijr}}{z}}
#' where \eqn{h_{ijr}} is the distance from each cell to the centroid of the patch
#' and \eqn{z} is the number of cells.
#'
#' GYRATE is an 'Area and edge metric'. The distance from each cell to the patch
#' centroid is based on cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0}
#' \subsection{Behaviour}{Approaches GYRATE = 0 if patch is a single cell. Increases,
#' without limit, when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
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

            points_class %>%
                dplyr::pull(id) %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_ij){

                    points_patch <- points_class %>%
                        dplyr::filter(id == patch_ij)

                    mean_x <- points_patch %>%
                        dplyr::pull(x) %>%
                        mean(na.rm = TRUE)

                    mean_y <- points_patch %>%
                        dplyr::pull(y) %>%
                        mean(na.rm = TRUE)

                    gyrate_patch <- raster::pointDistance(points_patch[, 1:2],
                                                          dplyr::bind_cols(x = mean_x,
                                                                           y = mean_y),
                                                          lonlat = FALSE) %>%
                        mean() %>%
                        magrittr::divide_by(nrow(points_patch))

                    tibble::tibble(level = "patch",
                                   class = as.integer(class),
                                   id = as.integer(patch_ij),
                                   metric = "radius of gyration",
                                   value = as.double(gyrate_patch))
                })
        })

}

