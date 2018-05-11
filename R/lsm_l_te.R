#'  Total Edge (TE)
#'
#' @description  Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_te(landscape)
#'
#' @aliases lsm_l_te
#' @rdname lsm_l_te
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.

lsm_l_te <- function(landscape){

    if (raster::nlayers(landscape) == 1) {
    # calculate total edge length by mapping over every class
    total_edge <- purrr::map(raster::unique(landscape), function(x) {

        # cclabel class
        cclabeled_raster <- cclabel(landscape, x)

        # set background to calculate number of neighbors next to cells with
        # values of -999
        cclabeled_raster[is.na(cclabeled_raster)] <- -999

        # compute neighborhood matrix
        adjacent_cells <- adjacent(cclabeled_raster,
                                   1:ncell(cclabeled_raster),
                                   4,
                                   pairs=TRUE)
        # count whos neighbor of who
        tb <- table(cclabeled_raster[adjacent_cells[,1]],
                    cclabeled_raster[adjacent_cells[,2]])


        # return first row with counts of adjents sites between patches and
        # cells with -999
        tb[2:ncol(tb),1]

    })

    total_edge <- tibble::tibble(
        layer = as.numeric(1),
        level = 'landscape',
        id = as.numeric(NA),
        metric = 'total edge',
        value = (sum(unlist(total_edge))/2) * prod(raster::res(landscape))
    )
    } else {
        ### stack
    }


    return(total_edge)

}
