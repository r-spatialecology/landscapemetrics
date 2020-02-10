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
get_circumscribingcircle <- function(landscape,
                                     directions,
                                     level) UseMethod("get_circumscribingcircle")

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterLayer <- function(landscape,
                                                 directions = 8,
                                                 level = "patch") {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_circumscribingcircle_calc,
                     directions = directions,
                     level = level)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterStack <- function(landscape,
                                                 directions = 8,
                                                 level = "patch") {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_circumscribingcircle_calc,
                     directions = directions,
                     level = level)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterBrick <- function(landscape,
                                                 directions = 8,
                                                 level = "patch") {

    result <- lapply(seq_along(raster::as.list(landscape)), function(x) {

        resolution_xy <- raster::res(landscape[[x]])
        resolution_x <- resolution_xy[[1]]
        resolution_y <- resolution_xy[[2]]

        mat <- raster::as.matrix(landscape[[x]])
        circle <- rcpp_get_circle(mat, resolution_xy = resolution_x)

        tibble::tibble(layer = x,
                       id = circle$patch_id,
                       dist = circle$circle_diameter)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.stars <- function(landscape,
                                           directions = 8,
                                           level = "patch") {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_circumscribingcircle_calc,
                     directions = directions,
                     level = level)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    return(result)
}

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.list <- function(landscape,
                                          directions = 8,
                                          level = "patch") {

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

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get resolution
        resolution <- raster::res(landscape)

        # get extent
        extent <- raster::extent(landscape)

        # convert to matrix
        landscape <- raster::as.matrix(landscape)
    }

    if (!exists("resolution")) {
        resolution <- c(1,1)
    } else {
        # check if resolution is identical
        if (resolution[1] != resolution[2]) {

            stop("The area of the circumscribing circle is currently only implemented for equal resolutions.",
                 call. = FALSE)
        }
    }

    # circle for each patch
    if (level == "patch") {

        # what classes are present
        classes <- get_unique_values(landscape)[[1]]

        # loop all classes
        circle <- do.call(rbind, lapply(classes, function(patches_class) {

            # get patches
            landscape_labeled <- get_patches(landscape,
                                             class = patches_class,
                                             directions = directions,
                                             to_disk = FALSE,
                                             return_raster = FALSE)[[1]]

            # get circle
            circle_patch <- rcpp_get_circle(landscape_labeled,
                                            resolution_xy = resolution[1])

            # resulting tibble
            circle_patch <- tibble::tibble(level = "patch",
                                           class = as.integer(patches_class),
                                           id = as.integer(circle_patch$patch_id),
                                           value = circle_patch$circle_diameter,
                                           center_x = circle_patch$circle_center_x,
                                           center_y = circle_patch$circle_center_y)}))
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

    if (exists("extent")) {
        # shift the coordinates to the original coordinate system
        circle$center_x = circle$center_x + extent@xmin
        circle$center_y = circle$center_y + extent@ymin
    }

    return(circle)
}
