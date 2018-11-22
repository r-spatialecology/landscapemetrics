#ifndef RCPP_CREATE_ANEIGHBORHOOD_H
#define RCPP_CREATE_ANEIGHBORHOOD_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

IntegerMatrix rcpp_create_neighborhood(arma::imat directions);

#endif // RCPP_CREATE_ANEIGHBORHOOD_H
