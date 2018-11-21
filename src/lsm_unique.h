#ifndef LSM_UNIQUE_H
#define LSM_UNIQUE_H
#include "Rcpp.h"

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::vector<int> lsm_unique(const Rcpp::IntegerVector &x);

#endif // LSM_UNIQUE_H
