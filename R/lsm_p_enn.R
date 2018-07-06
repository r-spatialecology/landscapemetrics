#' ENN (patch level)
#'
#' @description Euclidean Nearest-Neighbor Distance (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{ENN = h_{ij}}
#' where \eqn{h_{ij}} is the distance to the nearest neighbouring patch of
#' the same class i in meters
#'
#' ENN is an 'Aggregation metric'. The distance to the nearest neighbouring patch of
#' the same class i. The distance is measured from edge-to-edge. The range is limited by the
#' cell resolution on the lower limit and the landscape extent on the upper limit. The metric
#' is a simple way to describe patch isolation.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN > 0}
#' \subsection{Behaviour}{Approaches ENN = 0 as the distance to the nearest neighbour
#' decreases, i.e. patches of the same class i are more aggregated. Increases, without limit,
#' as the distance between neighbouring patches of the same class i increases, i.e. patches are
#' more isolated.}
#'
#' @seealso
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_enn(landscape)
#'
#' @aliases lsm_p_enn
#' @rdname lsm_p_enn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_enn <- function(landscape) UseMethod("lsm_p_enn")

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_enn
#' @export
lsm_p_enn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_enn
#' @export
lsm_p_enn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_enn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_enn_calc <- function(landscape) {

    landscape_labelled <- cclabel(landscape)

    enn_patch <- purrr::map_dfr(landscape_labelled, function(patches_class) {

        class <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        np_class <- patches_class %>%
            raster::values() %>%
            unique() %>%
            na.omit() %>%
            length()

        if(np_class == 1){
            minimum_distance <- as.double(NA)
            warning(paste0("Class ", class,
                           ": ENN = NA for class with only 1 patch"),
                    call. = FALSE)
        }

        else{
            points_class <- raster::rasterToPoints(patches_class)

            minimum_distance <- purrr::map_dbl(seq_len(np_class), function(patch_ij) {

                patch_focal <- points_class[points_class[,3] == patch_ij,]

                patch_others <- points_class[points_class[,3] != patch_ij,]

                minimum_distance <- dist(matrix(c(patch_focal[1:2],
                                                  patch_others[1:2]), ncol = 2, byrow = T)) %>%
                    min()
            })
        }

        tibble::tibble(class = class,
                       value = minimum_distance)

    })

    tibble::tibble(level = "patch",
                   class = as.integer(enn_patch$class),
                   id = as.integer(seq_len(nrow(enn_patch))),
                   metric = "euclidean nearest neighbor distance distribution (mean)",
                   value = as.double(enn_patch$value))

    # if(exists("print_warning")){
    #     warning("ENN = NA for class with only 1 patch")
    # }
    #
    # return(enn_patch)
}
