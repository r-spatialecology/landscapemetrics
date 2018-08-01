#' SHDI (landscape level)
#'
#' @description Shannon's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHID = - \sum \limits_{i = 1}^{m} (P_{i} * \ln P_{i})}
#' where \eqn{P_{i}} is the proportion of class i.
#'
#' SHDI is a 'Diversity metric'. It is a widely used metric in biodiversity and ecology
#' and takes both the number of classes and the abundance of each class into account.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHDI >= 0}
#' \subsection{Behaviour}{Equals SHDI = 0 when only one patch is present and increases,
#' without limit, as the number of classes increases while the proportions are
#' equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shdi(landscape)
#'
#' @aliases lsm_l_shdi
#' @rdname lsm_l_shdi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_shdi <- function(landscape)
    UseMethod("lsm_l_shdi")

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterBrick<- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_shdi_calc <- function(landscape) {
    area <- raster::ncell(landscape)

    p <- landscape %>%
        raster::values() %>%
        table() / area

    H <- tibble::tibble(
        level = 'landscape',
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shdi",
        value = as.double(sum(-p * log(p, exp(1))))
    )
}
