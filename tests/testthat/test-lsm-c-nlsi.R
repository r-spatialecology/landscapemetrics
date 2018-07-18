context("class level nlsi cv metric")

# fragstats_class_landscape_nlsi <- fragstats_class_landscape$NLSI

landscapemetrics_class_landscape_nlsi <- lsm_c_nlsi(landscape)

# test_that("lsm_c_nlsi results are equal to fragstats", {
#     expect_true(all(fragstats_class_landscape_nlsi %in%
#                         round(landscapemetrics_class_landscape_nlsi$value, 4)))
# })

test_that("lsm_c_nlsi is typestable", {
    expect_is(landscapemetrics_class_landscape_nlsi, "tbl_df")
    expect_is(lsm_c_nlsi(landscape_stack), "tbl_df")
    expect_is(lsm_c_nlsi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_nlsi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_nlsi), 6)
})

test_that("lsm_c_nlsi returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_nlsi$layer, "integer")
    expect_type(landscapemetrics_class_landscape_nlsi$level, "character")
    expect_type(landscapemetrics_class_landscape_nlsi$class, "integer")
    expect_type(landscapemetrics_class_landscape_nlsi$id, "integer")
    expect_type(landscapemetrics_class_landscape_nlsi$metric, "character")
    expect_type(landscapemetrics_class_landscape_nlsi$value, "double")
})

