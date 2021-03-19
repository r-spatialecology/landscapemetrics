#' get_circumscribingcircle
#'
#' @description Diameter of the circumscribing circle around patches
#'
#' @param landscape RasterLayer or matrix (with x, y, id columns)
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param level Either 'patch' or 'class' for the corresponding level.
#'
#' @details
#' The diameter of the smallest circumscribing circle around a patch in the landscape
#' is based on the maximum distance between the corners of each cell. This ensures that all
#' cells of the patch are included in the patch.
#'
#' @references
#' Based on C++ code from Project Nayuki (https://www.nayuki.io/page/smallest-enclosing-circle).
#'
#' @examples
#' # get circle around each patch
#' get_circumscribingcircle(landscape)
#'
#' # get circle around whole class
#' get_circumscribingcircle(landscape, level = "class")
#'
#' @aliases get_circumscribingcircle
#' @rdname get_circumscribingcircle
#'
#' @export
get_circumscribingcircle <- function(landscape, directions = 8, level = "patch") {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = get_circumscribingcircle_calc,
                     directions = directions,
                     level = level)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    return(result)

}

get_circumscribingcircle_calc <- function(landscape, level, directions) {

    # check level argument
    if (length(level) != 1 || !level %in% c("patch", "class")) {

        stop("The 'level' argument must be either 'patch' or 'class'.",
             call. = FALSE)
    }

    if (!inherits(x = landscape, what = "RasterLayer")) {
        stop("Please provide a 'RasterLayer', 'RasterStack', 'RasterBrick', 'stars'-object or a list with 'RasterLayers'.",
             call. = FALSE)
    }

    # get resolution
    resolution <- raster::res(landscape)

    # check if resolution is identical
    if (!isTRUE(all.equal(resolution[1], resolution[2]))) {

        stop("The area of the circumscribing circle is currently only implemented for equal resolutions.",
             call. = FALSE)
    }

    # get extent
    extent <- raster::extent(landscape)

    # convert to matrix
    landscape <- raster::as.matrix(landscape)

    # circle for each patch
    if (level == "patch") {

        # what classes are present
        classes <- get_unique_values(landscape)[[1]]

        # loop all classes
        circle <- do.call(rbind,
                          lapply(classes, function(patches_class) {
                              # get patches
                              landscape_labeled <- get_patches_int(landscape,
                                                                   class = patches_class,
                                                                   directions = directions)[[1]]

                              # get circle
                              cbind(class = as.integer(patches_class),
                                    rcpp_get_circle(landscape_labeled,
                                                    resolution_xy = resolution[1]))
                              })
                          )

        # resulting tibble
        circle <- tibble::tibble(level = "patch",
                                 class = as.integer(circle$class),
                                 id = as.integer(seq_len(nrow(circle))),
                                 value = circle$circle_diameter,
                                 center_x = circle$circle_center_x,
                                 center_y = circle$circle_center_y)
    }

    # class level (no labeling)
    else if (level == "class") {

        # get circle around classes
        circle_class <- rcpp_get_circle(landscape, resolution_xy = resolution[1])

        # resulting tibble
        circle <- tibble::tibble(level = "class",
                                 class = as.integer(circle_class$patch_id),
                                 id = as.integer(NA),
                                 value = circle_class$circle_diameter,
                                 center_x = circle_class$circle_center_x,
                                 center_y = circle_class$circle_center_y)
    }

    # shift the coordinates to the original coordinate system
    circle$center_x = circle$center_x + extent@xmin
    circle$center_y = circle$center_y + extent@ymin

    return(circle)
}
