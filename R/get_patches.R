#' get_patches
#'
#' @description Connected components labeling to derive patches in a landscape.
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param what Either "all" (default) for every class in the raster, or specify
#'             class value. See Details.
#'
#' @details
#' Searches for connected patches (neighbouring cells of the same class i).
#' The 8-neighbours rule ('queen's case) or 4-neighbours rule (rook's case) is
#' used. Returns a list with raster. For each class the connected patches have
#' the value 1 - n. All cells not belonging to the class are \code{NA}.
#' The underlying C code comes from the \code{SDMTools} package
#' (VanDerWal *et al.* 2014) and we appreciate their effort for implementing
#' this efficient connected labeling algorithm.
#'
#' @references
#' VanDerWal, J., Falconi, L., Januchowski, S., Shoo, L., and Storlie, C. 2014.
#' SDMTools: Species Distribution Modelling Tools: Tools for processing data
#' associated with species distribution modelling exercises.
#' R package version 1.1-221. <https://CRAN.R-project.org/package=SDMTools>
#'
#' Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time
#' component-labeling algorithm using contour tracing technique. Comput. Vis.
#' Image Underst. 93:206-220.
#'
#' @return List
#'
#' @examples
#' # check for patches of class 1
#' cclabeled_raster  <-  get_patches(landscape, 1)
#'
#' # count patches
#' length(raster::unique(cclabeled_raster[[1]]))
#'
#' # check for patches of every class
#' cclabeled_raster <-  get_patches(landscape)
#'
#' @aliases get_patches
#' @rdname get_patches
#'
#' @export
get_patches <- function(landscape, what, directions)  UseMethod("get_patches")


#' @name get_patches
#' @export
get_patches.RasterLayer <- function(landscape,
                                what = "all",
                                directions = 8) {
    get_patches_int(landscape,
                what = what,
                directions = directions) %>%
        raster::as.list()
}

#' @name get_patches
#' @export
get_patches.RasterStack <- function(landscape,
                                what = "all",
                                directions = 8) {
    purrr::map(raster::as.list(landscape_stack),
               .f = get_patches_int,
               what = what,
               directions = directions)

}

#' @name get_patches
#' @export
get_patches.RasterBrick <- function(landscape,
                                what = "all",
                                directions = 8) {
    purrr::map(raster::as.list(landscape),
               .f = get_patches_int,
               what = what,
               directions = directions)
}

#' @name get_patches
#' @export
get_patches.list <- function(landscape,
                         what = "all",
                         directions = 8) {
    purrr::map(landscape,
               .f = get_patches_int,
               what = what,
               directions = directions)
}

get_patches_int <- function(landscape, what, directions) {
    if (directions != 4 && directions != 8) {
        warning("You must specify a directions parameter. Defaulted to 8.",
                call. = FALSE)
        directions <- 8
    }

    landscape_extent <- raster::extent(landscape)

    landscape_empty <- raster::raster(
        x = landscape_extent,
        resolution = raster::res(landscape),
        crs = raster::crs(landscape)
    )

    filter_matrix <-
        matrix(NA,
               nrow = raster::nrow(landscape),
               ncol = raster::ncol(landscape))

    landscape_matrix <- raster::as.matrix(landscape)

    if (what != "all") {
        if (!isTRUE(what %in% raster::unique(landscape))) {
            stop(paste("There is no class", what, "in your raster"))
        }

        filter_matrix[landscape_matrix == what] <- 1

        if (directions == 4) {
            filter_raster = .Call('ccl_4', filter_matrix, PACKAGE = 'landscapemetrics')
        }

        if (directions == 8) {
            filter_raster = .Call('ccl_8', filter_matrix, PACKAGE = 'landscapemetrics')
        }

        cclabel_landscape <- raster::setValues(x = landscape_empty,
                                               values = filter_raster)

        names(cclabel_landscape) <- paste0("Class_", what)

        return(cclabel_landscape)
    }

    else {
        classes <- na.omit(unique(as.vector(landscape_matrix)))
        cclabel_landscape <- purrr::map(classes, function(what) {
            filter_matrix[landscape_matrix == what] <- 1

            if (directions == 4) {
                filter_raster = .Call('ccl_4', filter_matrix, PACKAGE = 'landscapemetrics')
            }

            if (directions == 8) {
                filter_raster = .Call('ccl_8', filter_matrix, PACKAGE = 'landscapemetrics')
            }

            cclabel_landscape <- raster::setValues(x = landscape_empty,
                                                   values = filter_raster)

            names(cclabel_landscape) <- paste0("Class_", what)

            cclabel_landscape
        })
        return(cclabel_landscape)

    }
}
