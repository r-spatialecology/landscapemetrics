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
#' @return RasterLayer/RasterStack
#'
#' @examples
#' # check for patches of class 1
#' cclabeled_raster <-  cclabel(landscape, 1)
#' raster::plot(cclabeled_raster)
#'
#' # count patches
#' length(raster::unique(cclabeled_raster))
#'
#' # check for patches of every class
#' cclabeled_raster <-  cclabel(landscape)
#' raster::plot(cclabeled_raster)
#'
#' #label a rasterstack
#' cclabeled_rasterstack <-  cclabel(landscape_stack)
#' raster::plot(cclabeled_rasterstack)
#'
#' @aliases cclabel
#' @rdname cclabel
#'
#' @export

cclabel <- function(landscape,
                    what = "all") {
    if (raster::nlayers(landscape) == 1) {
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
    } else {
        if (what != "all") {
            # coerce to matrix for connected labeling algorithm
            landscape_matrix_list <-
                purrr::map(seq(1, nlayers(landscape)),
                           function(x)
                               raster::as.matrix(landscape[[x]]))

            # ccl algorithm
            cclabel_matrix_list <-
                purrr::map(landscape_matrix_list, ccl_labels)
            cclabel_matrix_list <-
                purrr::map(cclabel_matrix_list, 1)

            # create filter matrix to only select connected regions that are
            #

            cclabel_landscape_list <-
                purrr::map(seq_along(cclabel_matrix_list), function(x) {
                    filter_mat <-
                        matrix(
                            FALSE,
                            nrow = raster::nrow(landscape),
                            ncol = raster::ncol(landscape)
                        )
                    filter_mat[landscape_matrix_list[[x]] == what] <-
                        TRUE

                    filtered_cclabel <-
                        ifelse(filter_mat, cclabel_matrix_list[[x]], NA)

                    cclabel_landscape <-
                        raster::raster(filtered_cclabel)

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

            names(cclabel_landscape_list) <-
                purrr::map_chr(seq_along(cclabel_landscape_list),
                               function(x) {
                                   paste("Layer", x)
                               })

            # return rasterstack for each class
            cclabel_landscape <-
                raster::brick(unlist(cclabel_landscape_list))

        } else {
            cclabel_landscape <-
                purrr::map(seq(1, nlayers(landscape)),
                           function(x) {
                               cclabel_list <-
                                   purrr::map(raster::unique(landscape[[x]]),
                                              function(y) {
                                                  # coerce to matrix for connected labeling algorithm
                                                  landscape_matrix <-
                                                      raster::as.matrix(landscape[[x]])
                                                  # ccl algorithm
                                                  cclabel_matrix <-
                                                      ccl_labels(as.matrix(landscape_matrix))[[1]]

                                                  # create filter matrix to only select connected regions that are
                                                  #
                                                  filter_mat <-
                                                      matrix(
                                                          FALSE,
                                                          nrow = raster::nrow(landscape),
                                                          ncol = raster::ncol(landscape)
                                                      )
                                                  filter_mat[landscape_matrix == y] <-
                                                      TRUE


                                                  filtered_cclabel <-
                                                      ifelse(filter_mat, cclabel_matrix, NA)

                                                  cclabel_landscape <-
                                                      raster::raster(filtered_cclabel)

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
                                   purrr::map_chr(seq_along(cclabel_list),
                                                  function(x) {
                                                      paste("Layer", x)
                                                  })

                               # return rasterstack for each class
                               cclabel_landscape <-
                                   raster::brick(unlist(cclabel_list))


                           })

        }
    }
    return(cclabel_landscape)
}
