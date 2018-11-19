#' get_patches
#'
#' @description Connected components labeling to derive patches in a landscape.
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param class Either "all" (default) for every class in the raster, or specify
#'             class value. See Details.
#' @param ccl_to_disk Logical argument, if FALSE results of get_patches are hold
#' in memory. If true, get_patches writes temporary files and hence, does not hold everything in memory.
#' Can be set with a global option, e.g. `option(ccl_to_disk = TRUE)`. See Details.
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
#' Landscape metrics rely on the delineation of patches. Hence, `get_patches` is
#' heavily used in **landscapemetrics**. As raster can be quite big, the fact that
#' `get_patches` creates a copy of the raster for each class in a landscape becomes
#' a burden for computer memory. Hence, the argument *ccl_to_disk* allows to
#' store the results of the connected labeling algorithm on disk. Furthermore,
#' this option can be set globally, so that every function that internally uses
#' `get_patches` can make use of that.
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
#' patched_raster  <-  get_patches(landscape, 1)
#'
#' # count patches
#' length(raster::unique(patched_raster[[1]]))
#'
#' # check for patches of every class
#' patched_raster <-  get_patches(landscape)
#'
#' @aliases get_patches
#' @rdname get_patches
#'
#' @export
get_patches <- function(landscape, class, directions, ccl_to_disk)  UseMethod("get_patches")


#' @name get_patches
#' @export
get_patches.RasterLayer <- function(landscape,
                                class = "all",
                                directions = 8,
                                ccl_to_disk = getOption("ccl_to_disk", default = FALSE)) {
    raster::as.list(get_patches_int(landscape,
                class = class,
                directions = directions,
                ccl_to_disk = ccl_to_disk))
}

#' @name get_patches
#' @export
get_patches.RasterStack <- function(landscape,
                                class = "all",
                                directions = 8,
                                ccl_to_disk = getOption("ccl_to_disk", default = FALSE)) {

    lapply(X = raster::as.list(landscape),
           FUN = get_patches_int,
           class = class,
           directions = directions,
           ccl_to_disk = ccl_to_disk)
}

#' @name get_patches
#' @export
get_patches.RasterBrick <- function(landscape,
                                class = "all",
                                directions = 8,
                                ccl_to_disk = getOption("ccl_to_disk", default = FALSE)) {

    lapply(X = raster::as.list(landscape),
           FUN = get_patches_int,
           class = class,
           directions = directions,
           ccl_to_disk = ccl_to_disk)
}

#' @name get_patches
#' @export
get_patches.list <- function(landscape,
                         class = "all",
                         directions = 8,
                         ccl_to_disk = getOption("ccl_to_disk", default = FALSE)) {

    lapply(X = landscape,
           FUN = get_patches_int,
           class = class,
           directions = directions,
           ccl_to_disk = ccl_to_disk)
}

get_patches_int <- function(landscape, class, directions, ccl_to_disk) {

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

    filter_matrix <- matrix(NA,
                            nrow = raster::nrow(landscape),
                            ncol = raster::ncol(landscape))

    landscape_matrix <- raster::as.matrix(landscape)

    if (class != "all") {

        if (!isTRUE(class %in% raster::unique(landscape))) {
            stop(paste("There is no class", class, "in your raster"))
        }

        filter_matrix[landscape_matrix == class] <- 1

        if (directions == 4) {
            filter_raster = .Call('ccl_4', filter_matrix, PACKAGE = 'landscapemetrics')
        }

        if (directions == 8) {
            filter_raster = .Call('ccl_8', filter_matrix, PACKAGE = 'landscapemetrics')
        }

        if(ccl_to_disk == TRUE){
            set_values <- function(x){filter_raster}
            patch_landscape <- raster::init(landscape_empty, fun=set_values, filename=tempfile(paste0("class_", class,".grd")), overwrite=TRUE)
        } else {
            patch_landscape <- raster::setValues(x = landscape_empty,
                                                 values = filter_raster)
        }

        names(patch_landscape) <- paste0("Class_", class)

        return(patch_landscape)
    }

    else {

        classes <- na.omit(unique(as.vector(landscape_matrix)))

        patch_landscape <- lapply(X = classes, FUN = function(class) {

            filter_matrix[landscape_matrix == class] <- 1

            if (directions == 4) {
                filter_raster = .Call('ccl_4', filter_matrix, PACKAGE = 'landscapemetrics')
            }

            if (directions == 8) {
                filter_raster = .Call('ccl_8', filter_matrix, PACKAGE = 'landscapemetrics')
            }


            if(ccl_to_disk == TRUE){
                set_values <- function(x){filter_raster}
                patch_landscape <- raster::init(landscape_empty, fun=set_values, filename=tempfile(paste0("class_", class,".grd")), overwrite=TRUE)
            } else {
                patch_landscape <- raster::setValues(x = landscape_empty,
                                                     values = filter_raster)
                }

            names(patch_landscape) <- paste0("Class_", class)

            patch_landscape

        })

        names(patch_landscape) <- sapply(patch_landscape, FUN = function(patches){

            as.numeric(strsplit(names(patches), "_")[[1]][2])

        })

        patch_landscape <- patch_landscape[order(as.numeric(names(patch_landscape)))]

        return(patch_landscape)
    }
}
