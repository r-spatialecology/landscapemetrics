#' PRD (landscape level)
#'
#' @description Patch richness density (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PRD = \frac{m} {A} * 10000 * 100 }
#' where \eqn{m} is the number of classes and \eqn{A} is the total landscape area in
#' square meters.
#'
#' PRD is a 'Diversity metric'. It is one of the simplest diversity and composition measures.
#' In contrast to \code{\link{lsm_l_pr}}, it is a relative measure and following, comparable
#' among landscapes with different total landscape areas.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{PR > 0}
#' \subsection{Behaviour}{Approaches PRD > 1 when only one patch is present and the landscape
#' is rather large. Increases, without limit, as the number of classes increases and the
#' total landscape area decreases.}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_prd(landscape)
#'
#' @aliases lsm_l_prd
#' @rdname lsm_l_prd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_l_prd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_prd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_prd_calc <- function(landscape, directions, resolution = NULL) {

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # summarise for total landscape
    area_total <- sum(area_patch$value)

    # all values NA
    if (is.na(area_total)) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "prd",
                              value = as.double(NA)))
    }

    # get number of classes
    pr_landscape <- lsm_l_pr_calc(landscape)

    # relative number of classes
    prd <- pr_landscape$value / area_total * 100

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "prd",
                          value = as.double(prd)))
}
