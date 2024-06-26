#' AI (class level)
#'
#' @description Aggregation index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#'
#' @details
#' \deqn{AI = \Bigg[\frac{g_{ii}}{max-g_{ii}} \Bigg](100) }
#'
#' where \eqn{g_{ii}} is the number of like adjacencies based on the single-count method and
#' \eqn{max-g_{ii}} is the classwise maximum number of like adjacencies of class i.
#'
#' AI is an 'Aggregation metric'. It equals the number of like adjacencies divided
#' by the theoretical maximum possible number of like adjacencies for that class.
#' The metric is based on he adjacency matrix and the the single-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= AI <= 100}
#' \subsection{Behaviour}{Equals 0 for maximally disaggregated and 100
#'  for maximally aggregated classes.}
#'
#' @return tibble
#'
#' @seealso
#' \code{\link{lsm_l_ai}}
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_ai(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' He, H. S., DeZonia, B. E., & Mladenoff, D. J. 2000. An aggregation index (AI)
#' to quantify spatial patterns of landscapes. Landscape ecology, 15(7), 591-601.
#'
#' @export
lsm_c_ai <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_ai_calc)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_ai_calc <- function(landscape, extras = NULL) {

    if (is.null(extras)){
        metrics <- "lsm_c_ai"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "ai",
                              value = as.double(NA))))
    }

    # get coocurrence matrix of like_adjacencies
    like_adjacencies <- rcpp_get_coocurrence_matrix_diag(landscape,
                                                         directions = as.matrix(4)) / 2

    # get number of cells each class
    cells_class <- extras$composition_vector

    # calculate maximum adjacencies
    n <- trunc(sqrt(cells_class))
    m <- cells_class - n ^ 2
    max_adj <- ifelse(test = m == 0, yes = 2 * n * (n - 1),
                      no = ifelse(test = m <= n, yes = 2 * n * (n - 1) + 2 * m - 1,
                                  no = ifelse(test = m > n, yes = 2 * n * (n - 1) + 2 * m - 2,
                                              no = NA)))

    # warning if NAs are introduced by ifelse
    if (anyNA(max_adj)) {
        stop("NAs introduced by lsm_c_ai", call. = FALSE)
    }

    # calculate aggregation index
    ai <- (like_adjacencies / max_adj) * 100

    # max_adj can be zero if only one cell is present; set to NA
    ai[is.nan(ai)] <- NA

    return(tibble::new_tibble(list(level = rep("class", length(ai)),
                          class = as.integer(names(like_adjacencies)),
                          id = rep(as.integer(NA), length(ai)),
                          metric = rep("ai", length(ai)),
                          value = as.double(ai))))
}
