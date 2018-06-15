prox_radius = 15

cclabeled_raster <- cclabel(landscape)

cclabel_points <- purrr::map(seq_along(cclabeled_raster),
                             function(x) {
                                 purrr::map(raster::unique(cclabeled_raster[[x]]),
                                            function(y) {
                                                raster::rasterToPoints(
                                                    cclabeled_raster[[x]],
                                                    fun = function(z) {
                                                        z == y
                                                    }
                                                )
                                            })
                             })


cclabel_points_dist <- purrr::map(seq_along(cclabel_points),
                                  function(x) {
                                      purrr::map(seq_along(cclabel_points[[x]]), function(y) {
                                          purrr::map(seq_along(cclabel_points[[x]]), function(z) {
                                              x <- raster::pointDistance(cclabel_points[[x]][[y]][, 1:2],
                                                                         cclabel_points[[x]][[z]][, 1:2],
                                                                         lonlat = FALSE)
                                              tibble::as.tibble(x)
                                          })
                                      })
                                  })



cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_dist),function(x) {
    purrr::map(seq_along(cclabel_points_dist[[x]]), function(y) {
        purrr::map(seq_along(cclabel_points_dist[[x]][[y]]), function(z) {

            sum(cclabel_points_dist[[x]][[y]][[z]] > prox_radius) / min(cclabel_points_dist[[x]][[y]][[z]])

        })
    })
})

PROX equals the sum of patch area (m2) divided by the nearest edge-to-edge distance squared (m2) between the patch and the focal patch of all patches of the corresponding patch type whose edges are within a specified distance (m) of the focal patch.PROX equals the sum of patch area (m2) divided by the nearest edge-to-edge distance squared (m2) between the patch and the focal patch of all patches of the corresponding patch type whose edges are within a specified distance (m) of the focal patch.PROX equals the sum of patch area (m2) divided by the nearest edge-to-edge distance squared (m2) between the patch and the focal patch of all patches of the corresponding patch type whose edges are within a specified distance (m) of the focal patch.cclabel_points_prox  <- purrr::map(seq_along(cclabel_points_prox),function(x) {
    purrr::map(seq_along(cclabel_points_prox[[x]]), function(y) {
        sum(unlist(cclabel_points_prox[[x]][[y]]), na.rm = T)
    })
})


tibble::tibble(
    level = "patch",
    class = unlist(purrr::map(seq_along(cclabel_points_prox), function(x){
        rep(x, length(cclabel_points_prox[[x]]))
    })),
    id = seq_len(length(unlist(cclabel_points_prox))),
    metric = "proximity index",
    value = unlist(cclabel_points_prox)
) %>% View()
