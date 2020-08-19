#' nLSI (class level)
#'
#' @description Normalized landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{nLSI = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces
#'
#' nLSI is an 'Aggregation metric'. It is the ratio between the actual edge length of
#' class i and the hypothetical minimum edge length of class i. The minimum edge length equals
#' the edge length if class i would be maximally aggregated.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{nlsi >= 1}
#' \subsection{Behaviour}{Equals nlsi = 1 when only one squared patch is present or all
#' patches are maximally aggregated. Increases, without limit, as the length of the
#' actual edges increases, i.e. the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_nlsi(landscape)
#'
#' @aliases lsm_c_nlsi
#' @rdname lsm_c_nlsi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_c_nlsi <- function(landscape, directions = 8) {
    landscape <- lsm_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_nlsi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_nlsi_calc <- function(landscape, directions, resolution = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- raster::res(landscape)

        landscape <- raster::as.matrix(landscape)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "nlsi",
                              value = as.double(NA)))
    }

    # get edge for each class
    class_edge <- lsm_c_te_calc(landscape,
                                directions = directions,
                                count_boundary = TRUE,
                                resolution = resolution)

    # get total edge
    total_edge <- lsm_l_te_calc(landscape,
                                count_boundary = TRUE,
                                resolution = resolution)

    ai <- rcpp_get_composition_vector(landscape)

    pi <- prop.table(ai)

    A <- sum(ai)
    B <- (ncol(landscape) * 2) + (nrow(landscape) * 2)
    Z <- total_edge$value

    nlsi <- tibble::tibble(ai = ai,
                           pi = pi,
                           A  = A,
                           B  = B,
                           Z  = Z)

    nlsi$n <- trunc(sqrt(nlsi$ai))
    nlsi$m <- nlsi$ai - nlsi$n ^ 2
    nlsi$min_e <- ifelse(test = nlsi$m == 0,
                         yes = nlsi$n * 4,
                         no = ifelse(test = nlsi$n ^ 2 < nlsi$ai & nlsi$ai <= nlsi$n * (1 + nlsi$n),
                                     yes = 4 * nlsi$n + 2,
                                     no = ifelse(test = nlsi$ai > nlsi$n * (1 + nlsi$n),
                                                 yes = 4 * nlsi$n + 4,
                                                 no = NA)))

    nlsi$max_e <- ifelse(test = nlsi$pi <= 0.5,
                         yes = 4 * nlsi$ai,
                         no = ifelse(test = nlsi$A %% 2 == 0 & nlsi$pi > .5 & nlsi$pi <= (.5 * nlsi$A + .5 * nlsi$B) / nlsi$A,
                                     yes = 3 * nlsi$A - 2 * nlsi$ai,
                                     no = ifelse(test = nlsi$A %% 2 != 0 & nlsi$pi > .5 & nlsi$pi <= (.5 * nlsi$A + .5 * nlsi$B) / nlsi$A,
                                                 yes = 3 * nlsi$A - 2 * nlsi$ai + 3,
                                                 no = ifelse(test = nlsi$pi >= (.5 * nlsi$A + .5 * nlsi$B) / nlsi$A,
                                                             yes = nlsi$Z + 4 * (nlsi$A - nlsi$ai),
                                                             no = NA))))

    result <- (class_edge$value - nlsi$min_e) / (nlsi$max_e - nlsi$min_e)
    result[is.nan(result)] <- NA

    return(tibble::tibble(level = "class",
                          class = as.integer(class_edge$class),
                          id = as.integer(NA),
                          metric = "nlsi",
                          value = as.double(result)))
}
