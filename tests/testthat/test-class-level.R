# context("class level calc")
#
# fs_class = data.frame(
#     metric = c(
#         "total area",
#         "patch area (mean)",
#         "patch area (mean)",
#         "patch area (mean)",
#         "patch area (cv)",
#         "patch area (cv)",
#         "patch area (cv)",
#         "patch area (sd)",
#         "patch area (sd)",
#         "patch area (sd)",
#         "percentage of landscape",
#         "percentage of landscape",
#         "percentage of landscape",
#         "largest patch index",
#         "largest patch index",
#         "largest patch index",
#         "number of patches",
#         "number of patches",
#         "number of patches",
#         "euclidean nearest neighbor distance distribution (mean)",
#         "euclidean nearest neighbor distance distribution (mean)",
#         "euclidean nearest neighbor distance distribution (mean)"
#     ),
#     class = c(NA, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2,
#               1, 3, 2, 1, 3, 2),
#     value_fs = c(
#         NA,
#         0.002,
#         0.012,
#         0.0017,
#         228.5906,
#         162.6136,
#         159.0109,
#         0.0045,
#         0.0195,
#         0.0027,
#         19.8889,
#         53.2222,
#         26.8889,
#         16.4444,
#         50.7778,
#         10.8889,
#         9,
#         4,
#         14,
#         3.6829,
#         2,
#         3.1969
#     )
# )
#
# lsm_class = lsm_calculate(landscape, what = "class")
#
# class = left_join(fs_class, lsm_class, by = c("metric", "class"))
# a = as.list(class$value_fs)
# b = as.list(class$value)
# names(a) = paste0("Metric: ", class$metric, ", Class: ", class$class)
# names(b) = paste0("Metric: ", class$metric, ", Class: ", class$class)
#
# test_that("class level results are equal to fragstats", {
#     expect_equal(a, b, tolerance = 0.01)
# })
