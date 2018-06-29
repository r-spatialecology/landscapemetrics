#' NP (class level)
#'
#' @description Number of patches (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{NP = n_{i}}
#' where \eqn{n_{i}} is the number of patches.
#'
#' NP is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{NP >= 1}
#' \subsection{Behaviour}{Equals NP = 1 when only one patch is present and
#' increases, without limit, as the number of patches increases}
#'
#' @seealso
#' \code{\link{lsm_l_np}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_np(landscape)
#'
#' @aliases lsm_c_np
#' @rdname lsm_c_np
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_np <- function(landscape) UseMethod("lsm_c_np")

#' @name lsm_c_np
#' @export
lsm_c_np.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_np
#' @export
lsm_c_np.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_np
#' @export
lsm_c_np.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_np
#' @export
lsm_c_np.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_np_calc <- function(landscape){

    landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(patches_class){

            class <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            np <- patches_class %>%
                raster::values() %>%
                unique() %>%
                na.omit() %>%
                length()

            tibble::tibble(
                level = "class",
                class = as.integer(class),
                id = as.integer(NA),
                metric = "number of patches",
                value = as.double(np)
            )
        })
}
