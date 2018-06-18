context("patch level area metric")

fragstats_values <- fragstats_patch_landscape$AREA
landscapemetrics_values <- lsm_p_area(landscape)$value

test_that("landscape level results are equal to fragstats", {
        expect_true(all(fragstats_values %in% landscapemetrics_values))
})
