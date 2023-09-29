#' PARA_MN (class level)
#'
#' @description Mean perimeter-area ratio (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA_{MN} = mean(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_MN is a 'Shape metric'. It summarises each class as the mean of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_MN > 0}
#' \subsection{Behaviour}{Approaches PARA_MN > 0 if PARA for each patch approaches PARA > 0,
#' i.e. the form approaches a rather small square. Increases, without limit, as PARA increases,
#' i.e. patches become more complex.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_para_mn(landscape)
#'
#' @aliases lsm_c_para_mn
#' @rdname lsm_c_para_mn
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_para_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_para_mn_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_para_mn_calc <- function(landscape, directions, resolution, extras = NULL){

    para <- lsm_p_para_calc(landscape,
                            directions = directions,
                            resolution = resolution,
                            extras = extras)

    # all cells are NA
    if (all(is.na(para$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "para_mn",
                              value = as.double(NA))))
    }

    para_mn <- stats::aggregate(x = para[, 5], by = para[, 2], FUN = mean)

    return(tibble::new_tibble(list(level = rep("class", nrow(para_mn)),
                              class = as.integer(para_mn$class),
                              id = rep(as.integer(NA), nrow(para_mn)),
                              metric = rep("para_mn", nrow(para_mn)),
                              value = as.double(para_mn$value))))
}
