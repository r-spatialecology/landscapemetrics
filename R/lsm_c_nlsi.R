#' nlsi (class level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{nlsi = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces
#'
#' nlsi is an 'Aggregation metric'. It is the ratio between the actual edge length of
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
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_l_nlsi}}
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
#' @export
#' lsm_c_nlsi <- function(landscape) UseMethod("lsm_c_nlsi")
#'
#' #' @name lsm_c_nlsi
#' #' @export
#' lsm_c_nlsi.RasterLayer <- function(landscape) {
#'     purrr::map_dfr(raster::as.list(landscape), lsm_c_nlsi_calc, .id = "layer") %>%
#'         dplyr::mutate(layer = as.integer(layer))
#' }
#'
#' #' @name lsm_c_nlsi
#' #' @export
#' lsm_c_nlsi.RasterStack <- function(landscape) {
#'     purrr::map_dfr(raster::as.list(landscape), lsm_c_nlsi_calc, .id = "layer") %>%
#'         dplyr::mutate(layer = as.integer(layer))
#' }
#'
#' #' @name lsm_c_nlsi
#' #' @export
#' lsm_c_nlsi.RasterBrick <- function(landscape) {
#'     purrr::map_dfr(raster::as.list(landscape), lsm_c_nlsi_calc, .id = "layer") %>%
#'         dplyr::mutate(layer = as.integer(layer))
#' }
#'
#' #' @name lsm_c_nlsi
#' #' @export
#' lsm_c_nlsi.list <- function(landscape) {
#'     purrr::map_dfr(landscape, lsm_c_nlsi_calc, .id = "layer") %>%
#'         dplyr::mutate(layer = as.integer(layer))
#' }
#'
#' lsm_c_nlsi_calc <- function(landscape) {
#'
#'     edge_class <- lsm_c_te_calc(landscape, count_boundary = T)
#'
#'     edge_landscape <- lsm_l_te_calc(landscape, count_boundary = T) %>%
#'         dplyr::pull(value)
#'
#'     number_cells <- length(na.omit(raster::values(landscape)))
#'
#'     number_cells_boundary <- (raster::nrow(landscape) * 2) +
#'         (raster::ncol(landscape) * 2)
#'
#'     area_class <- landscape %>%
#'         lsm_c_ca_calc() %>%
#'         dplyr::mutate(value = value * 10000)
#'
#'     proportion_class <- lsm_c_pland(landscape) %>%
#'         dplyr::mutate(value = value / 100)
#'
#'     dplyr::full_join(x = area_class, y = proportion_class, by = "class")
#'
#'     min_e <- dplyr::mutate(area_class,
#'                          n = trunc(sqrt(value)),
#'                          m = value - n^ 2,
#'                          min_e = dplyr::case_when(m == 0 ~ n * 4,
#'                                                  n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
#'                                                  value > n * (1 + n) ~ 4 * n + 4))
#'
#'
#'     max_e <- dplyr::mutate(proportion_class,
#'                            max_e = dplyr::case_when(value <= 0.5 ~ 0,
#'                                                     value > (0.5 * number_cells + 0.5 * number_cells_boundary) /
#'                                                         number_cells ~ edge_landscape + 4 * (number_cells - a_i)))
#'
#'     tibble::tibble(
#'         level = "patch",
#'         class = as.integer(edge_class$class),
#'         id = as.integer(edge_class$id),
#'         metric = "nlsi",
#'         value = as.double(nlsi$value)
#'     )
#'
#' }
