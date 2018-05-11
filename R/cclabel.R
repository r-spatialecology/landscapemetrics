#' Connected compoment labeling for classes in a raster
#'
#' @description xxx
#'
#' @param landscape xxx
#' @param what Either "all" for every class in the raster, or specify class value. See Details.
#'
#' @details
#'
#' `what` returns a RasterStack if the argument `"all"` is used. Each layer
#' represents a unique class found in the raster and contains the labelled patches.
#'
#' @return RasterLayer/RasterStack
#'
#' @examples
#' cclabeled_raster <-  cclabel(landscape, 1)
#' raster::plot(cclabeled_raster)
#'
#' # count patches
#' length(raster::unique(cclabeled_raster))
#' @aliases cclabel
#' @rdname cclabel
#'
#' @export

cclabel <- function(landscape,
                    what) {


    if(what != "all") {
    # coerce to matrix for connected labeling algorithm
    landscape_matrix <- raster::as.matrix(landscape)
    # ccl algorithm
    cclabel_matrix <- ccl_labels(as.matrix(landscape_matrix))[[1]]

    # create filter matrix to only select connected regions that are
    #
    filter_mat <- matrix(FALSE, nrow = raster::nrow(landscape), ncol = raster::ncol(landscape))
    filter_mat[landscape_matrix == what] <- TRUE


    filtered_cclabel <- ifelse(filter_mat, cclabel_matrix, NA)

    cclabel_landscape <- raster::raster(filtered_cclabel)

    rcl <-  cbind(raster::unique(cclabel_landscape),
                  raster::unique(cclabel_landscape),
                  seq(1, length(raster::unique(cclabel_landscape))))



    cclabel_landscape <-  raster::reclassify(cclabel_landscape,
                                     rcl = rcl,
                                     right = NA)


    } else {
        #####
    }

    return(cclabel_landscape)
}
