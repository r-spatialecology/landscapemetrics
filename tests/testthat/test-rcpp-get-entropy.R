context("entropy")

buys <- c("no", "no", "yes", "yes", "yes", "no", "yes",
         "no", "yes", "yes", "yes", "yes", "yes", "no")
freqs <- table(buys) / length(buys)

e_true1 <- -sum(freqs * log2(freqs))
e_true2 <- -sum(freqs * log(freqs))
e_true3 <- -sum(freqs * log10(freqs))

e_result1 <- rcpp_get_entropy(freqs, "log2")
e_result2 <- rcpp_get_entropy(freqs, "log")
e_result3 <- rcpp_get_entropy(freqs, "log10")

test_that("rcpp_get_entropy results are correct", {
    expect_equivalent(e_result1, e_true1)
    expect_equivalent(e_result2, e_true2)
    expect_equivalent(e_result3, e_true3)
})

test_that("rcpp_get_entropy is typestable", {
    expect_is(e_result1, "numeric")
})
