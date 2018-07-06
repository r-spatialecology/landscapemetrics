#' CIRCLE (patch level)
#'
#' @description Related Circumscribing Circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CIRCLE = 1 - (\frac{a_{ij}} {a_{ij}^{circle}})}
#' where \eqn{a_{ij}} is the area in square meters and \eqn{a_{ij}^{circle}} the area of
#' the smalles circumscribing circle.
#'
#' CIRCLE is a 'Shape metric'. The metric is the ratio between the patch area and the smallest
#' circumscribing circle of the patch. The diameter of the smallest circumscribing circle is
#' the 'diameter' of the patch connecting the opposing corner points of the two cells
#' that are the furthest away from each other. The metric characterises the compactness
#' of the patch and is comparable among patches with different area.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= CIRCLE < 1}
#' \subsection{Behaviour}{CIRCLE = 0 for a circular patch and approaches CIRCLE = 1 for
#' a linear patch.}
#'
#' @seealso
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
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
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
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

    resolution <- landscape %>%
        raster::res() %>%
        magrittr::extract2(1) %>%
        magrittr::divide_by(2)

    area_patch <- lsm_p_area_calc(landscape)

    landscape_labelled <- cclabel(landscape)

    circle_patch <- purrr::map_dfr(landscape_labelled, function(patches_class) {

        class <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        points_class <- patches_class %>%
            raster::rasterToPoints() %>%
            tibble::as.tibble() %>%
            purrr::set_names(c("x", "y", "id"))

        points_class %>%
            dplyr::pull(id) %>%
            unique() %>%
            sort() %>%
            purrr::map_dfr(function(patch_ij){

                x <- points_class %>%
                    dplyr::filter(id == patch_ij) %>%
                    dplyr::pull(x)

                y <- points_class %>%
                    dplyr::filter(id == patch_ij) %>%
                    dplyr::pull(y)

                points_corners <- matrix(c(x = c(x - resolution,  x - resolution,
                                                 x + resolution,  x + resolution),
                                           y = c(y - resolution,  y + resolution,
                                                 y + resolution,  y - resolution)), ncol = 2)

                diameter <- points_corners %>%
                    dist() %>%
                    max()

                circle <- (diameter / 2) ^ 2  * pi

                tibble::tibble(class = class,
                               value = circle)
            })
    })

    circle_patch <- dplyr::mutate(circle_patch,
                                  value = 1 - ((area_patch$value * 10000) / value))

    tibble::tibble(
        level = "patch",
        class = as.integer(circle_patch$class),
        id = as.integer(seq_len(nrow(circle_patch))),
        metric = "related circumscribing circle",
        value = as.double(circle_patch$value)
    )
}




