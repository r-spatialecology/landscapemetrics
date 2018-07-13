#' SHEI (landscape level)
#'
#' @description Shannons's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SHEI = \frac{- \sum \limits_{i = 1} ^ {m} (P_{i} * \ln P_{i})} {\ln m}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SHEI is a 'Diversity metric'. It is the ratio between the actual Shannon's diversity index
#' and and the theoretical maximum of the Shannon diversity index. It can be understood as a
#' measure of dominance.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= SHEI < 1}
#' \subsection{Behaviour}{Equals SHEI = 0 when only one patch  present and equals SHEI = 1
#' when the proportion of classes is completely equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shei(landscape)
#'
#' @aliases lsm_l_shei
#' @rdname lsm_l_shei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_shei <- function(landscape) UseMethod("lsm_l_shei")

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterLayer <- function(landscape){
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterStack <- function(landscape){
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterBrick <- function(landscape){
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.list <- function(landscape){
    purrr::map_dfr(landscape, lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_shei_calc <- function(landscape){
    area <- raster::ncell(landscape)

    p <- landscape %>%
        raster::values() %>%
        table() / area

    E <- tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shei",
        value = as.double(sum(-p * log(p, exp(1)), na.rm = TRUE) /
                              log(length(p), exp(1)))
    )
    E
}
