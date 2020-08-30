#' PLADJ (landscape level)
#'
#' @description Percentage of Like Adjacencies (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#'
#' @details
#' \deqn{PLADJ = (\frac{g_{ij}} {\sum \limits_{k = 1}^{m} g_{ik}}) * 100}
#' where \eqn{g_{ii}} is the number of adjacencies between cells of class i
#' and \eqn{g_{ik}} is the number of adjacencies between cells of class i and k.
#'
#' PLADJ is an 'Aggregation metric'. It calculates the frequency how often patches of
#' different classes i (focal class) and k are next to each other, and following is a
#' measure of class aggregation. The adjacencies are counted using the double-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{0 <= PLADJ <= 100}
#' \subsection{Behaviour}{Equals PLADJ = 0 if class i is maximal disaggregated,
#' i.e. every cell is a different patch. Equals PLADJ = 100 when the only one patch
#' is present.}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pladj(landscape)
#'
#' @aliases lsm_l_pladj
#' @rdname lsm_l_pladj
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html.
#'
#' @export
lsm_l_pladj <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_pladj_calc <- function(landscape) {

    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "pladj",
                              value = as.double(NA)))
    }

    landscape_padded <- pad_raster(landscape,
                                   pad_raster_value = -999,
                                   pad_raster_cells = 1,
                                   return_raster = FALSE)[[1]]

    tb <- rcpp_get_coocurrence_matrix(landscape_padded,
                                      directions = as.matrix(4))

    like_adjacencies <- sum(diag(tb)[-1])
    total_adjacencies <- sum(tb[,-1])

    pladj <- like_adjacencies / total_adjacencies * 100

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "pladj",
                          value = as.double(pladj)))
}
