#' GYRATE (patch level)
#'
#' @description Radius of Gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{GYRATE = \sum \limits_{r = 1}^{z} \frac{h_{ijr}} {z}}
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
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
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

    landscape_labelled <- cclabel(landscape)

    purrr::map_dfr(landscape_labelled, function(patches_class) {

            class <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            points_class <- patches_class %>%
                raster::rasterToPoints() %>%
                tibble::as.tibble() %>%
                purrr::set_names(c("x", "y", "id"))

            patches_class <- points_class %>%
                dplyr::pull(id) %>%
                unique() %>%
                sort()

            purrr::map_dfr(patches_class, function(patch_ij){

                points_patch <- dplyr::filter(points_class, id == patch_ij)

                mean_x <- points_patch %>%
                    dplyr::pull(x) %>%
                    mean(na.rm = TRUE)

                mean_y <- points_patch %>%
                    dplyr::pull(y) %>%
                    mean(na.rm = TRUE)

                gyrate_patch <- raster::pointDistance(points_patch[, 1:2],
                                                      dplyr::bind_cols(x = mean_x,
                                                                       y = mean_y),
                                                      lonlat = FALSE)
                gyrate_mean_patch <- mean(gyrate_patch, na.rm = TRUE)

                tibble::tibble(level = "patch",
                               class = as.integer(class),
                               id = as.integer(patch_ij),
                               metric = "radius of gyration",
                               value = as.double(gyrate_mean_patch))
            })
    })

}

