#ifndef rcpp_get_unique_values_H
#define rcpp_get_unique_values_H
#include "Rcpp.h"

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::vector<int> rcpp_get_unique_values(const Rcpp::IntegerVector &x, bool na_omit = true);

#endif // rcpp_get_unique_values_H
