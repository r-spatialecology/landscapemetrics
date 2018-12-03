#' AI (landscape level)
#'
#' @description Aggregation index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#'
#' @details
#' \deqn{AI = \Bigg[\sum\limits_{i=1}^m \Big( \frac{g_{ii}}{max-g_{ii}} \Big) P_{i} \Bigg](100) }
#'
#' where \eqn{g_{ii}} is the number of like adjacencies based on the single-count method and
#' \eqn{max-g_{ii}} is the classwise maximum number of like adjacencies of class i and \eqn{P_{i}}
#' the proportion of landscape compromised of class i.
#'
#' AI is an 'Aggregation metric'. It equals the number of like adjacencies divided
#' by the theoretical maximum possible number of like adjacencies for that class summed over each
#' class for the entire landscape.
#' The metric is based on he adjacency matrix and the the single-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= AI <= 100}
#' \subsection{Behaviour}{Equals 0 for maximally disaggregated and 100
#'  for maximally aggregated classes.}
#'
#' @return tibble
#' @seealso
#' \code{\link{lsm_c_ai}}
#'
#' @examples
#' lsm_l_ai(landscape)
#'
#' @aliases lsm_l_ai
#' @rdname lsm_l_ai
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' He, H. S., DeZonia, B. E., & Mladenoff, D. J. 2000. An aggregation index (AI)
#' to quantify spatial patterns of landscapes. Landscape ecology, 15(7), 591-601.
#'
#' @export
lsm_l_ai <- function(landscape) UseMethod("lsm_l_ai")

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterLayer <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_ai_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_ai_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterBrick <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_ai_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_ai
#' @export
lsm_l_ai.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_ai_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_ai
#' @export
lsm_l_ai.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_ai_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_ai_calc <- function(landscape, resolution = NULL) {

    # convert to matrix
    if(class(landscape) != "matrix") {
        resolution <- raster::res(landscape)
        landscape <- raster::as.matrix(landscape)
    }

    # get aggregation index for each class
    ai <- lsm_c_ai_calc(landscape)

    # get proportional class area
    pland <- lsm_c_pland_calc(landscape,
                              directions = 8,
                              resolution = resolution)

    # final AI index
    result <- sum(ai$value * (pland$value / 100))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "ai",
        value = as.double(result)
    )
}

