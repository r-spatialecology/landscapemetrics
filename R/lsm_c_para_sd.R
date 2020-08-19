#' PARA_SD (class level)
#'
#' @description Standard deviation perimeter-area ratio (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA_{SD} = sd(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_SD is a 'Shape metric'. It summarises each class as the standard deviation of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_SD >= 0}
#' \subsection{Behaviour}{Equals PARA_SD = 0 if the perimeter-area ratio is identical for
#' all patches. Increases, without limit, as the variation of the perimeter-area ratio
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_sd(landscape)
#'
#' @aliases lsm_c_para_sd
#' @rdname lsm_c_para_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_para_sd <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_para_sd_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_para_sd_calc <- function(landscape, directions, resolution = NULL){

    para <- lsm_p_para_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # all cells are NA
    if (all(is.na(para$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "para_sd",
                              value = as.double(NA)))
    }

    para_sd <- stats::aggregate(x = para[, 5], by = para[, 2],
                                FUN = stats::sd)

    return(tibble::tibble(level = "class",
                          class = as.integer(para_sd$class),
                          id = as.integer(NA),
                          metric = "para_sd",
                          value = as.double(para_sd$value)))
}
