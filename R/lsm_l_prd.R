#' PRD (landscape level)
#'
#' @description Patch richness density (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_prd(landscape)
#'
#' @aliases lsm_l_prd
#' @rdname lsm_l_prd
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
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

lsm_l_prd_calc <- function(landscape, directions, resolution, extras = NULL) {

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    # summarise for total landscape
    area_total <- sum(area_patch$value)

    # all values NA
    if (is.na(area_total)) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "prd",
                              value = as.double(NA))))
    }

    # get number of classes
    pr_landscape <- lsm_l_pr_calc(landscape, extras = extras)

    # relative number of classes
    prd <- pr_landscape$value / area_total * 100

    return(tibble::new_tibble(list(level = rep("landscape", length(prd)),
                          class = rep(as.integer(NA), length(prd)),
                          id = rep(as.integer(NA), length(prd)),
                          metric = rep("prd", length(prd)),
                          value = as.double(prd))))
}
