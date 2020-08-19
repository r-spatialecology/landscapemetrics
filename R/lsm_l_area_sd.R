#' AREA_SD (landscape level)
#'
#' @description Standard deviation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{SD} = sd(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_SD is an 'Area and Edge metric'. The metric summarises the landscape
#' as the standard deviation of all patch in the landscape. The metric describes
#' the differences among all patches in the landscape.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_SD >= 0}
#' \subsection{Behaviour}{Equals AREA_SD = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}} \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_sd(landscape)
#'
#' @aliases lsm_l_area_sd
#' @rdname lsm_l_area_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_area_sd <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_area_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

# Not working yet!
lsm_l_area_sd_calc <- function(landscape, directions, resolution = NULL){

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # all values NA
    if (all(is.na(area_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "area_sd",
                              value = as.double(NA)))
    }

    # calculate sd
    area_sd <- stats::sd(area_patch$value)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "area_sd",
                          value = as.double(area_sd)))
}


