#' COHESION (landscape level)
#'
#' @description Patch Cohesion Index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{COHESION = 1 - (\frac{\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} p_{ij}} {\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} p_{ij} \sqrt{a_{ij}}}) * (1 - \frac{1} {\sqrt{Z}}) ^ {-1} * 100}
#' where \eqn{p_{ij}} is the perimeter in meters, \eqn{a_{ij}} is the area in square
#' meters and \eqn{Z} is the number of cells.
#'
#' COHESION is an 'Aggregation metric'.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{Unknown}
#' \subsection{Behaviour}{Unknown}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_l_cohesion}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_cohesion(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Schumaker, N. H. 1996. Using landscape indices to predict habitat
#' connectivity. Ecology, 77(4), 1210-1225.
#'
#' @export
lsm_l_cohesion <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_cohesion_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_cohesion_calc <- function(landscape, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_l_cohesion"
        resolution <- terra::res(landscape)
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "cohesion",
                              value = as.double(NA))))
    }

    # get number of cells
    ncells_landscape <- length(landscape[!is.na(landscape)])

    # get number of cells in each patch: area = n_cells * res / 10000
    ncells_patch <- lsm_p_area_calc(landscape,
                                    directions = directions,
                                    resolution = resolution,
                                    extras = extras)

    ncells_patch$value <- ncells_patch$value * 10000 / prod(resolution)

    # get perim for each patch
    perim_patch <- lsm_p_perim_calc(landscape,
                                    directions = directions,
                                    resolution = resolution,
                                    extras = extras)

    # denominator for cohesion (perim / n_cells) for landscape
    denominator <- sum(perim_patch$value * sqrt(ncells_patch$value))

    # calcualte cohesion
    cohesion <- (1 - (sum(perim_patch$value) / denominator)) *
        ((1 - (1 / sqrt(ncells_landscape))) ^ -1) * 100

    return(tibble::new_tibble(list(level = rep("landscape", length(cohesion)),
                 class = rep(as.integer(NA), length(cohesion)),
                 id = rep(as.integer(NA), length(cohesion)),
                 metric = rep("cohesion", length(cohesion)),
                 value = as.double(cohesion))))
}
