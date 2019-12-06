#' PLADJ (class level)
#'
#' @description Percentage of Like Adjacencies (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' lsm_c_pladj(landscape)
#'
#' @aliases lsm_c_pladj
#' @rdname lsm_c_pladj
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html.
#'
#' @export
lsm_c_pladj <- function(landscape)
    UseMethod("lsm_c_pladj")

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterLayer <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterBrick <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_pladj_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pladj_calc <- function(landscape) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all cells are NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "class",
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

    pladj <- vapply(X = seq_len(nrow(tb)), FUN = function(x) {
        like_adjacencies <- tb[x, x]
        total_adjacencies <- sum(tb[x, ])

        like_adjacencies / total_adjacencies * 100
    }, FUN.VALUE = numeric(1))

    pladj <- pladj[-1]
    names <- row.names(tb)[-1]

    return(tibble::tibble(level = "class",
                          class = as.integer(names),
                          id = as.integer(NA),
                          metric = "pladj",
                          value = as.double(pladj)))
}
