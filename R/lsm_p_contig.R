#' contig (patch level)
#'
#' @description contig area (contig area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{contig = a_{ij}^{contig}}
#' where \eqn{a_{ij}^{contig}} is the contig area in square meters
#'
#' contig is a 'contig area metric' and equals the area within a patch that is not
#' on the edge of it. A cell is defined as contig area if the cell has no
#' neighbour with a different value than itself (rook's case). It describes patch area
#' and shape simultaneously (more contig area when the patch is large and the shape is
#' rather compact, i.e. a square).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{contig >= 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch area increases
#' and the patch shape simplifies (more contig area). contig = 0 when every cell in
#' the patch is an edge.}
#'
#' @seealso
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#'
#' @examples
#' lsm_p_contig(landscape)
#'
#' @aliases lsm_p_contig
#' @rdname lsm_p_contig
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_contig <- function(landscape) UseMethod("lsm_p_contig")

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc,.id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_contig
#' @export
lsm_p_contig.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_contig_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}


lsm_p_contig_calc <- function(landscape) {

    filter_matrix <- matrix(c(1, 2, 1,
                              2, 1, 2,
                              1, 2, 1), 3, 3, byrow = T)

    filter_function <- function(x) {
        center <- x[ceiling(length(x) / 2)]

        if (is.na(center)) {
            return(center)
        } else {
            sum(x, na.rm = TRUE)
        }
    }


    contig_patch <- landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(patches_class) {
            patches_class %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_id) {
                    patches_class[patches_class != patch_id] <- NA
                    patches_class[patches_class == patch_id] <- 1

                    filter <- raster::focal(patches_class,
                                            filter_matrix,
                                            filter_function,
                                            pad = TRUE)

                    contig <- ((raster::cellStats(filter, sum) /
                                    sum(!is.na(getValues(patches_class)))
                                ) - 1) / 12

                    class_name <- patches_class %>%
                        names() %>%
                        sub("Class_", "", .)

                    tibble::tibble(class = class_name,
                                   value = contig)
                })
        })

    tibble::tibble(
        level = "patch",
        class = as.integer(contig_patch$class),
        id = as.integer(seq_len(nrow(contig_patch))),
        metric = "contiguity",
        value = as.double(contig_patch$value)
    )
}
