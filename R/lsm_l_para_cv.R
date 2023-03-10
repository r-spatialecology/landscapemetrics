#' PARA_CV (landscape level)
#'
#' @description Coefficient of variation perimeter-area ratio (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA_{CV} = cv(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_CV is a 'Shape metric'. It summarises the landscape as the Coefficient of variation of
#' each patch belonging in the landscape The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_CV >= 0}
#' \subsection{Behaviour}{Equals PARA_CV = 0 if the perimeter-area ratio is identical for
#' all patches. Increases, without limit, as the variation of the perimeter-area ratio
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_para}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_para_cv(terra::unwrap(landscape))
#'
#' @aliases lsm_l_para_cv
#' @rdname lsm_l_para_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_l_para_cv <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_para_cv_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_para_cv_calc <- function(landscape, directions, resolution = NULL){

    para_patch <- lsm_p_para_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # all values NA
    if (all(is.na(para_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "para_cv",
                              value = as.double(NA)))
    }

    para_cv <- sd(para_patch$value) / mean(para_patch$value) * 100

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "para_cv",
                          value = as.double(para_cv)))
}
