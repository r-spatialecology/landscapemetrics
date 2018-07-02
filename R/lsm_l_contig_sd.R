#' CONTIG_SD (landscape level)
#'
#' @description Standard deviation of Contiguity index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CONTIG_{SD} =  sd(CONTIG[patch_{ij}])}
#'
#' where \eqn{CONTIG[patch_{ij}]} is the contiguity of each patch.
#'
#' CONTIG_SD is a 'Shape metric'. It summarises the landscape as the standard deviation of all patches
#' in the landscape. CONTIG_SD asses the spatial connectedness (contiguity) of
#' cells in patches. The metric coerces patch values to a value of 1 and the background
#' to NA. A nine cell focal filter matrix:
#'
#' ```
#' filter_matrix <- matrix(c(1, 2, 1,
#'                           2, 1, 2,
#'                           1, 2, 1), 3, 3, byrow = T)
#' ```
#' ... is then used to weight orthogonally contiguous pixels more heavily than
#' diagonally contiguous pixels. Therefore, larger and more connections between
#' patch cells in the rookie case result in larger contiguity index values.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CONTIG_SD >= 0}
#' \subsection{Behaviour}{CONTIG_SD = 0 if the contiguity index is
#' identical for all patches. Increases, without limit, as the variation of
#' CONTIG increases.}
#'
#' @seealso
#' \code{\link{lsm_p_contig}},
#' \code{\link{lsm_c_contig_sd}},
#' \code{\link{lsm_c_contig_cv}},
#' \code{\link{lsm_c_contig_mn}}, \cr
#' \code{\link{lsm_l_contig_cv}},
#' \code{\link{lsm_l_contig_mn}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_contig_sd(landscape)
#'
#' @aliases lsm_l_contig_sd
#' @rdname lsm_l_contig_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_contig_sd <- function(landscape) UseMethod("lsm_l_contig_sd")

#' @name lsm_l_contig_sd
#' @export
lsm_l_contig_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_contig_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_contig_sd
#' @export
lsm_l_contig_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_contig_sd_calc,.id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_contig_sd
#' @export
lsm_l_contig_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_contig_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_contig_sd
#' @export
lsm_l_contig_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_contig_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_contig_sd_calc <- function(landscape) {

    contig_cv  <- landscape %>%
        lsm_p_contig_calc() %>%
        dplyr::summarize(value = sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "contiguity distribution (cv)",
        value = as.double(contig_cv$value)
    )

}
