#' SHEI (landscape level)
#'
#' @description Shannons's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#'
#' @details
#' \deqn{SHEI = \frac{- \sum \limits_{i = 1} ^ {m} (P_{i} * \ln P_{i})} {\ln m}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SHEI is a 'Diversity metric'. It is the ratio between the actual Shannon's diversity index
#' and and the theoretical maximum of the Shannon diversity index. It can be understood as a
#' measure of dominance.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= SHEI < 1}
#' \subsection{Behaviour}{Equals SHEI = 0 when only one patch  present and equals SHEI = 1
#' when the proportion of classes is completely equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shei(landscape)
#'
#' @aliases lsm_l_shei
#' @rdname lsm_l_shei
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Shannon, C., and W. Weaver. 1949. The mathematical theory of
#' communication. Univ. IllinoisPress, Urbana
#'
#' @export
lsm_l_shei <- function(landscape){
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_shei_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_shei_calc <- function(landscape, resolution = NULL){

    # get class proportions (direction doesn't matter)
    prop <- lsm_c_pland_calc(landscape,
                             directions = 8,
                             resolution = resolution)

    # all values NA
    if (all(is.na(prop$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "shei",
                              value = as.double(NA)))
    }

    prop <- prop$value / 100

    shei <- sum(-prop * log(prop, exp(1))) / log(length(prop), exp(1))

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "shei",
                          value = as.double(shei)))
}
