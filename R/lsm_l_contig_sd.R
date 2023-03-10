#' CONTIG_SD (landscape level)
#'
#' @description Standard deviation of Contiguity index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CONTIG_{SD} =  sd(CONTIG[patch_{ij}])}
#'
#' where \eqn{CONTIG[patch_{ij}]} is the contiguity of each patch.
#'
#' CONTIG_SD is a 'Shape metric'. It summarises the landscape as the standard deviation of all patches
#' in the landscape. CONTIG_SD asses the spatial connectedness (contiguity) of
#' cells in patches. The metric coerces patch values to a value of 1 and the background
#' to NA. A nine cell focal filter matrix:
#'
#' ```
#' filter_matrix <- matrix(c(1, 2, 1,
#'                           2, 1, 2,
#'                           1, 2, 1), 3, 3, byrow = T)
#' ```
#' ... is then used to weight orthogonally contiguous pixels more heavily than
#' diagonally contiguous pixels. Therefore, larger and more connections between
#' patch cells in the rookie case result in larger contiguity index values.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CONTIG_SD >= 0}
#' \subsection{Behaviour}{CONTIG_SD = 0 if the contiguity index is
#' identical for all patches. Increases, without limit, as the variation of
#' CONTIG increases.}
#'
#' @seealso
#' \code{\link{lsm_p_contig}},
#' \code{\link{lsm_c_contig_sd}},
#' \code{\link{lsm_c_contig_cv}},
#' \code{\link{lsm_c_contig_mn}}, \cr
#' \code{\link{lsm_l_contig_cv}},
#' \code{\link{lsm_l_contig_mn}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_contig_sd(terra::unwrap(landscape))
#'
#' @aliases lsm_l_contig_sd
#' @rdname lsm_l_contig_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' LaGro, J. 1991. Assessing patch shape in landscape mosaics.
#' Photogrammetric Engineering and Remote Sensing, 57(3), 285-293
#'
#' @export
lsm_l_contig_sd <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_contig_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_contig_sd_calc <- function(landscape, directions) {

    contig_patch <- lsm_p_contig_calc(landscape,
                                      directions = directions)

    # all values NA
    if (all(is.na(contig_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "contig_sd",
                              value = as.double(NA)))
    }

    contig_sd <- stats::sd(contig_patch$value)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "contig_sd",
                          value = as.double(contig_sd)))
}
