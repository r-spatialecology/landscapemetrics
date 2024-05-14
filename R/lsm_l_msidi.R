#' MSIDI (landscape level)
#'
#' @description Modified Simpson's diversity index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MSIDI = -\ln \sum \limits_{i = 1}^{m} P_{i}^{2}}
#' where \eqn{P_{i}} is the landscape area proportion of class i.
#'
#' MSIDI is a 'Diversity metric'. Because the metric is based on distances or areas
#' please make sure your data is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{MSIDI >= 0}
#' \subsection{Behaviour}{MSIDI = 0 if only one patch is present and increases, without
#' limit, as the amount of patches with equally distributed landscape proportions increases}
#'
#' @seealso
#' \code{\link{lsm_l_sidi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_msidi(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' Pielou, E. C. 1975. Ecological Diversity. Wiley-Interscience, New York.
#'
#' Romme, W. H. 1982. Fire and landscapediversity in subalpine forests of
#' Yellowstone National Park.Ecol.Monogr. 52:199-221
#'
#' @export
lsm_l_msidi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_msidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_msidi_calc <- function(landscape, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_l_msidi"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "msidi",
                              value = as.double(NA))))
    }

    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution,
                                  extras = extras)

    msidi <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2], FUN = sum)

    msidi <- -log(sum((msidi$value / sum(msidi$value)) ^ 2))

    return(tibble::new_tibble(list(level = rep("landscape", length(msidi)),
                          class = rep(as.integer(NA), length(msidi)),
                          id = rep(as.integer(NA), length(msidi)),
                          metric = rep("msidi", length(msidi)),
                          value = as.double(msidi))))
}
