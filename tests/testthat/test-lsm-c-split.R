context("class level split metric")

fragstats_class_landscape_split <- fragstats_class_landscape$SPLIT
landscapemetrics_class_landscape_split <- lsm_c_split(landscape)

test_that("lsm_c_split results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_split %in%
                        round(landscapemetrics_class_landscape_split$value, 4)))
})

test_that("lsm_c_split is typestable", {
    expect_is(landscapemetrics_class_landscape_split, "tbl_df")
    expect_is(lsm_c_split(landscape_stack), "tbl_df")
    expect_is(lsm_c_split(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_split returns the desirsplit number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_split), 6)
})

test_that("lsm_c_split returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_split$layer, "integer")
    expect_type(landscapemetrics_class_landscape_split$level, "character")
    expect_type(landscapemetrics_class_landscape_split$class, "integer")
    expect_type(landscapemetrics_class_landscape_split$id, "integer")
    expect_type(landscapemetrics_class_landscape_split$metric, "character")
    expect_type(landscapemetrics_class_landscape_split$value, "double")
})


