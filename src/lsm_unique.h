#ifndef LSM_UNIQUE_H
#define LSM_UNIQUE_H
#include "Rcpp.h"

std::vector<int> lsm_unique(const Rcpp::IntegerMatrix &x);
std::vector<int> lsm_unique(const Rcpp::IntegerVector &x);


#endif // LSM_UNIQUE_H
