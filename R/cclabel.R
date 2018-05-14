#' Connected compoment labeling for classes in a raster
#'
#' @description xxx
#'
#' @param landscape xxx
#' @param what Either "all" (default) for every class in the raster, or specify
#'             class value. See Details.
#'
#' @details
#'
#' `what` returns a RasterBrick if the argument `"all"` is used. Each layer
#' represents a unique class found in the raster and contains the labelled patches.
#'
#' @return List with RasterLayer/RasterBrick
#'
#' @examples
#' # check for patches of class 1
#' cclabeled_raster  <-  cclabel(landscape, 1)
#'
#' # count patches
#' length(raster::unique(cclabeled_raster[[1]]))
#'
#' # check for patches of every class
#' cclabeled_raster <-  cclabel(landscape)
#'
#' #label a rasterstack
#' cclabeled_rasterstack <-  cclabel(landscape_stack)
#'
#' @aliases cclabel
#' @rdname cclabel
#'
#' @export
cclabel <- function(landscape, what = "all")  UseMethod("cclabel")


#' @name cclabel
#' @export
cclabel.RasterLayer <- function(landscape, what = "all") {
    cclabel_int(landscape, what) %>%
        raster::as.list()
}

#' @name cclabel
#' @export
cclabel.RasterStack <- function(landscape, what = "all") {
    purrr::map(raster::as.list(landscape), .f = cclabel_int, what = what)
}

#' @name cclabel
#' @export
cclabel.RasterBrick <- function(landscape, what = "all") {
    purrr::map(raster::as.list(landscape), .f = cclabel_int, what = what)
}

#' @name cclabel
#' @export
cclabel.list <- function(landscape, what = "all") {
    purrr::map(landscape, .f = cclabel_int, what = what)
}

cclabel_int <- function(landscape, what = "all") {

    if (what != "all") {
        # coerce to matrix for connected labeling algorithm
        landscape_matrix <- raster::as.matrix(landscape)
        # ccl algorithm
        cclabel_matrix <-
            ccl_labels(landscape_matrix)[[1]]

        # create filter matrix to only select connected regions that are
        #
        filter_mat <-
            matrix(
                FALSE,
                nrow = raster::nrow(landscape),
                ncol = raster::ncol(landscape)
            )
        filter_mat[landscape_matrix == what] <- TRUE


        filtered_cclabel <-
            ifelse(filter_mat, cclabel_matrix, NA)

        cclabel_landscape <- raster::raster(filtered_cclabel)

        # specify resolution ----
        raster::extent(cclabel_landscape) <- c(
            raster::xmin(landscape),
            raster::xmax(landscape) * raster::res(landscape)[1],
            raster::xmin(landscape),
            raster::xmax(landscape) * raster::res(landscape)[2]
        )

        rcl <-  cbind(
            raster::unique(cclabel_landscape),
            raster::unique(cclabel_landscape),
            seq_along(raster::unique(cclabel_landscape))
        )

        cclabel_landscape <-
            raster::reclassify(cclabel_landscape,
                               rcl = rcl,
                               right = NA)

    } else {
        cclabel_list <- purrr::map(raster::unique(landscape), function(x) {
            # coerce to matrix for connected labeling algorithm
            landscape_matrix <- raster::as.matrix(landscape)
            # ccl algorithm
            cclabel_matrix <-
                ccl_labels(landscape_matrix)[[1]]

            # create filter matrix to only select connected regions that are
            #
            filter_mat <-
                matrix(
                    FALSE,
                    nrow = raster::nrow(landscape),
                    ncol = raster::ncol(landscape)
                )
            filter_mat[landscape_matrix == x] <- TRUE


            filtered_cclabel <-
                ifelse(filter_mat, cclabel_matrix, NA)

            cclabel_landscape <-
                raster::raster(filtered_cclabel)

            # specify resolution ----
            raster::extent(cclabel_landscape) <- c(
                raster::xmin(landscape),
                raster::xmax(landscape) * raster::res(landscape)[1],
                raster::xmin(landscape),
                raster::xmax(landscape) * raster::res(landscape)[2]
            )

            rcl <-  cbind(
                raster::unique(cclabel_landscape),
                raster::unique(cclabel_landscape),
                seq_along(raster::unique(cclabel_landscape))
            )

            cclabel_landscape <-
                raster::reclassify(cclabel_landscape,
                                   rcl = rcl,
                                   right = NA)
        })

        names(cclabel_list) <-
            purrr::map_chr(raster::unique(landscape),
                           function(x) {
                               paste("Class", x)
                           })

        # return rasterstack for each class
        cclabel_landscape <-
            raster::brick(unlist(cclabel_list))


    }


}
