#' PR (landscape level)
#'
#' @description Patch richness (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PR = m}
#' where \eqn{m} is the number of classes
#'
#' PR is a 'Diversity metric'. It is one of the simplest diversity and composition measures.
#' However, because of its absolute nature, it is not comparable among landscapes with
#' different total areas.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PR >= 1}
#' \subsection{Behaviour}{Equals PR = 1 when only one patch is present and increases, without
#' limit, as the number of classes increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pr(landscape)
#'
#' @aliases lsm_l_pr
#' @rdname lsm_l_pr
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export

lsm_l_pr <- function(landscape) UseMethod("lsm_l_pr")

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterLayer <- function(landscape){

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pr_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterStack <- function(landscape){

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pr_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterBrick <- function(landscape){

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pr_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pr_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.list <- function(landscape){

    result <- lapply(X = landscape,
                     FUN = lsm_l_pr_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_pr_calc <- function(landscape){

    richness <- length(get_unique_values(landscape)[[1]])

    tibble::tibble(
        level = 'landscape',
        class = as.integer(NA),
        id = as.integer(NA),
        metric = 'pr',
        value = as.double(richness))

}
