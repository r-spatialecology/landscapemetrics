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
#' cclabeled_raster <-  cclabel(landscape, 0.5)
#' plot(cclabeled_raster)
#'
#' # count patches
#' length(unique(cclabeled_raster))
#' @aliases cclabel
#' @rdname cclabel
#'
#' @export

cclabel <- function(landscape,
                    what) {


    if(what != "all") {
    # coerce to matrix for connected labeling algorithm
    landscape_matrix <- as.matrix(landscape)
    # ccl algorithm
    cclabel_matrix <- ccl_labels(as.matrix(landscape_matrix))[[1]]

    # create filter matrix to only select connected regions that are
    #
    landscape_matrix[landscape_matrix != what] <- FALSE
    landscape_matrix[landscape_matrix == what] <- TRUE

    filtered_cclabel <- ifelse(landscape_matrix, cclabel_matrix, NA)

    cclabel_landscape <- raster(filtered_cclabel)

    } else {
        #####
    }

    return(cclabel_landscape)
}
