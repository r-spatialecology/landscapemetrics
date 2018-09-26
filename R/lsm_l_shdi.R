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
#' Shannon, C., and W. Weaver. 1949. The mathematical theory of
#' communication. Univ. IllinoisPress, Urbana
#'
#' @export
lsm_l_shdi <- function(landscape)
    UseMethod("lsm_l_shdi")

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterLayer <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shdi_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shdi_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterBrick<- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shdi_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_shdi_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}


#' @name lsm_l_shdi
#' @export
lsm_l_shdi.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_shdi_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_shdi_calc <- function(landscape) {

    area <- raster::ncell(landscape)

    p <- table(raster::values(landscape)) / area

    H <- tibble::tibble(
        level = 'landscape',
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "shdi",
        value = as.double(sum(-p * log(p, exp(1))))
    )
}
