#' AI (landscape level)
#'
#' @description Aggregation index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
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
#' The metric is based on he adjacency matrix and the single-count method.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_ai(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' He, H. S., DeZonia, B. E., & Mladenoff, D. J. 2000. An aggregation index (AI)
#' to quantify spatial patterns of landscapes. Landscape ecology, 15(7), 591-601.
#'
#' @export
lsm_l_ai <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     directions = directions,
                     FUN = lsm_l_ai_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ai_calc <- function(landscape, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_l_ai"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ai",
                              value = as.double(NA))))
    }

    # get aggregation index for each class
    ai <- lsm_c_ai_calc(landscape, extras = extras)

    # get proportional class area
    pland <- lsm_c_pland_calc(landscape,
                              directions = 8,
                              resolution = resolution,
                              extras = extras)

    # final AI index
    ai <- sum(ai$value * (pland$value / 100), na.rm = TRUE)

    return(tibble::new_tibble(list(level = rep("landscape", length(ai)),
                 class = rep(as.integer(NA), length(ai)),
                 id = rep(as.integer(NA), length(ai)),
                 metric = rep("ai", length(ai)),
                 value = as.double(ai))))
}

