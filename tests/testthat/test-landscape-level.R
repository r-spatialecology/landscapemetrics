context("landscape level calc")

fs_land = data.frame(
    metric = c(
        "total area",
        "patch area (mean)",
        "patch area (cv)",
        "patch area (sd)",
        "largest patch index",
        "total edge",
        "number of patches",
        "patch richness",
        "patch richness density",
        "relative patch richness",
        "euclidean nearest neighbor distance distribution (mean)",
        "Shannon's evenness index",
        "Shannon's diversity index"
    ),
    value_fs = c(
        0.09,
        0.0033,
        268.3046,
        0.0089,
        50.7778,
        364,
        27,
        3,
        3333.3333,
        NA,
        .1816,
        0.9194,
        1.0101
    )
)

lsm_land = lsm_calculate(landscape, what = "landscape")

land = left_join(fs_land, lsm_land, by = "metric")
a = as.list(land$value_fs)
b = as.list(land$value)
names(a) = land$metric
names(b) = land$metric

test_that("landscape level results are equal to fragstats", {
    expect_equal(a, b, tolerance = 0.01)
})
