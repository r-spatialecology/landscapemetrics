#' SHDI (landscape level)
#'
#' @description Shannon's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#'
#' @details
#' \deqn{SHDI = - \sum \limits_{i = 1}^{m} (P_{i} * \ln P_{i})}
#' where \eqn{P_{i}} is the proportion of class i.
#'
#' SHDI is a 'Diversity metric'. It is a widely used metric in biodiversity and ecology
#' and takes both the number of classes and the abundance of each class into account.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHDI >= 0}
#' \subsection{Behaviour}{Equals SHDI = 0 when only one patch is present and increases,
#' without limit, as the number of classes increases while the proportions are
#' equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shdi(landscape)
#'
#' @aliases lsm_l_shdi
#' @rdname lsm_l_shdi
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
lsm_l_shdi <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_shdi_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_shdi_calc <- function(landscape, resolution = NULL) {

    # get class proportions (direction doesn't matter)
    prop <- lsm_c_pland_calc(landscape,
                             directions = 8,
                             resolution = resolution)

    # all values NA
    if (all(is.na(prop$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "shdi",
                              value = as.double(NA)))
    }

    prop <- prop$value / 100

    shdi <- sum(-prop * log(prop, exp(1)))

    return(tibble::tibble(level = 'landscape',
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "shdi",
                          value = as.double(shdi)))
}
