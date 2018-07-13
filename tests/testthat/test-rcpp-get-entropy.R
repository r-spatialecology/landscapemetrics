context("entropy")

buys <- c("no", "no", "yes", "yes", "yes", "no", "yes",
         "no", "yes", "yes", "yes", "yes", "yes", "no")
freqs <- table(buys) / length(buys)

e_true1 <- -sum(freqs * log2(freqs))
e_true2 <- -sum(freqs * log(freqs))

e_result1 <- landscapemetrics:::rcpp_get_entropy(freqs, "log2")
e_result2 <- landscapemetrics:::rcpp_get_entropy(freqs, "log")

test_that("rcpp_get_entropy results are correct", {
    expect_equivalent(e_result1, e_true1)
    expect_equivalent(e_result2, e_true2)
})

test_that("rcpp_get_entropy is typestable", {
    expect_is(e_result1, "numeric")
})
