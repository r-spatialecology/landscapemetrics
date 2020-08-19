#' PARA_CV (class level)
#'
#' @description Coefficient of variation perimeter-area ratio (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA_{CV} = cv(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_CV is a 'Shape metric'. It summarises each class as the Coefficient of variation of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
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
#' \code{\link{lsm_p_para}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_cv(landscape)
#'
#' @aliases lsm_c_para_cv
#' @rdname lsm_c_para_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_para_cv <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_para_cv_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_para_cv_calc <- function(landscape, directions, resolution = NULL){

    para <- lsm_p_para_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # all cells are NA
    if (all(is.na(para$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "para_cv",
                              value = as.double(NA)))
    }

    para_cv <- stats::aggregate(x = para[, 5], by = para[, 2], FUN = raster::cv)

    return(tibble::tibble(level = "class",
                          class = as.integer(para_cv$class),
                          id = as.integer(NA),
                          metric = "para_cv",
                          value = as.double(para_cv$value)))
}
