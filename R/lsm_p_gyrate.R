#' GYRATE (patch level)
#'
#' @description Radius of Gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{GYRATE = \sum \limits_{r = 1}^{z} \frac{h_{ijr}} {z}}
#' where \eqn{h_{ijr}} is the distance from each cell to the centroid of the
#' patch and \eqn{z} is the number of cells.
#'
#' GYRATE is an 'Area and edge metric'. The distance from each cell to the
#' patch
#' centroid is based on cell center-to-cell center distances. The metrics
#' characterises both the patch area and compactness.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0}
#' \subsection{Behaviour}{Approaches GYRATE = 0 if patch is a single cell.
#' Increases, without limit, when only one patch is present.}
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
lsm_p_gyrate <- function(landscape, directions) UseMethod("lsm_p_gyrate")

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_gyrate_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_gyrate_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_gyrate_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_p_gyrate_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_gyrate_calc <- function(landscape, directions) {

    landscape_labelled <- get_patches(landscape, directions = directions)

    gyrate <- purrr::map_dfr(landscape_labelled, function(patches_class) {

        class <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        points_class <- patches_class %>%
            raster::rasterToPoints() %>%
            tibble::as.tibble() %>%
            purrr::set_names(c("x", "y", "id"))

        centroid <- points_class %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(x_centroid = mean(x),
                             y_centroid = mean(y))

        full_data <- dplyr::left_join(x = points_class, y = centroid, by = "id")

        gyrate_class <-  dplyr::mutate(full_data, dist =
                                           sqrt((x - x_centroid) ^ 2 +
                                                    (y - y_centroid) ^ 2)) %>%
            dplyr::group_by(id) %>%
            dplyr::summarise(value = mean(dist, na.rm = TRUE))

        tibble::tibble(class = as.integer(class),
                       value = as.double(gyrate_class$value))
    })

    tibble::tibble(level = "patch",
                   class = as.integer(gyrate$class),
                   id = as.integer(seq_len(nrow(gyrate))),
                   metric = "gyrate",
                   value = as.double(gyrate$value))

}

